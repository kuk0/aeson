-- WARNING: This file is security sensitive as it uses unsafeWrite which does
-- not check bounds. Any changes should be made with care and we would love to
-- get informed about them, just cc us in any PR targetting this file: @eskimor @jprider63
-- We would be happy to review the changes!

-- The security check at the end (pos > length) only works if pos grows
-- monotonously, if this condition does not hold, the check is flawed.
module Data.Aeson.Parser.UnescapePure
    (
      unescapeText
    ) where

import Control.Exception (evaluate, throw, try)
import Control.Monad (when)
import Data.ByteString as B
import Data.Bits (shiftL, (.|.))
import Data.Text (Text)
import qualified Data.Text.Array as A
import qualified Data.Text.Internal.Unsafe.Char as C
import qualified Data.Text.Internal.Encoding.Utf16 as U16
import Data.Text.Encoding.Error (UnicodeException (..))
import Data.Text.Internal.Private (runText)
import Data.Text.Unsafe (unsafeDupablePerformIO)
import Data.Word (Word8, Word16)
import GHC.ST (ST)

-- Different UTF states.
data Utf =
      UtfGround
    | UtfTail1
    | UtfU32e0
    | UtfTail2
    | UtfU32ed
    | Utf843f0
    | UtfTail3
    | Utf843f4
    deriving (Eq, Show)

data State =
      StateNone
    | StateUtf !Utf
    | StateBackslash
    | StateU0
    | StateU1 !Word16
    | StateU2 !Word16
    | StateU3 !Word16
    | StateS0 !Word16
    | StateS1 !Word16
    | StateSU0 !Word16
    | StateSU1 !Word16 !Word16
    | StateSU2 !Word16 !Word16
    | StateSU3 !Word16 !Word16
    deriving (Eq, Show)

-- References:
-- http://bjoern.hoehrmann.de/utf-8/decoder/dfa/
-- https://github.com/jwilm/vte/blob/master/utf8parse/src/table.rs.in

decode :: Utf -> Word8 -> State
decode UtfGround word = case word of
    w | 0x00 <= w && w <= 0x7f -> StateNone
    w | 0xc2 <= w && w <= 0xdf -> StateUtf UtfTail1
    0xe0                       -> StateUtf UtfU32e0
    w | 0xe1 <= w && w <= 0xec -> StateUtf UtfTail2
    0xed                       -> StateUtf UtfU32ed
    w | 0xee <= w && w <= 0xef -> StateUtf UtfTail2
    0xf0                       -> StateUtf Utf843f0
    w | 0xf1 <= w && w <= 0xf3 -> StateUtf UtfTail3
    0xf4                       -> StateUtf Utf843f4
    _                          -> throwDecodeError

decode UtfU32e0 word = case word of
    w | 0xa0 <= w && w <= 0xbf -> StateUtf UtfTail1
    _                          -> throwDecodeError

decode UtfU32ed word = case word of
    w | 0x80 <= w && w <= 0x9f -> StateUtf UtfTail1
    _                          -> throwDecodeError

decode Utf843f0 word = case word of
    w | 0x90 <= w && w <= 0xbf -> StateUtf UtfTail2
    _                          -> throwDecodeError

decode Utf843f4 word = case word of
    w | 0x80 <= w && w <= 0x8f -> StateUtf UtfTail2
    _                          -> throwDecodeError

decode UtfTail3 word = case word of
    w | 0x80 <= w && w <= 0xbf -> StateUtf UtfTail2
    _                          -> throwDecodeError

decode UtfTail2 word = case word of
    w | 0x80 <= w && w <= 0xbf -> StateUtf UtfTail1
    _                          -> throwDecodeError

decode UtfTail1 word = case word of
    w | 0x80 <= w && w <= 0xbf -> StateNone
    _                          -> throwDecodeError

{-# INLINE decode #-}

decodeHex :: Word8 -> Word16
decodeHex 48  = 0  -- '0'
decodeHex 49  = 1  -- '1'
decodeHex 50  = 2  -- '2'
decodeHex 51  = 3  -- '3'
decodeHex 52  = 4  -- '4'
decodeHex 53  = 5  -- '5'
decodeHex 54  = 6  -- '6'
decodeHex 55  = 7  -- '7'
decodeHex 56  = 8  -- '8'
decodeHex 57  = 9  -- '9'
decodeHex 65  = 10 -- 'A'
decodeHex 97  = 10 -- 'a'
decodeHex 66  = 11 -- 'B'
decodeHex 98  = 11 -- 'b'
decodeHex 67  = 12 -- 'C'
decodeHex 99  = 12 -- 'c'
decodeHex 68  = 13 -- 'D'
decodeHex 100 = 13 -- 'd'
decodeHex 69  = 14 -- 'E'
decodeHex 101 = 14 -- 'e'
decodeHex 70  = 15 -- 'F'
decodeHex 102 = 15 -- 'f'
decodeHex _ = throwDecodeError
{-# INLINE decodeHex #-}

unescapeText' :: ByteString -> Text
unescapeText' bs = runText $ \done -> do
    dest <- A.new len
    (pos, finalState) <- B.foldl' (f' dest) (return (0, StateNone)) bs

    -- Check final state. Currently pos gets only increased over time, so this check should catch overflows.
    when ( finalState /= StateNone || pos > len)
      throwDecodeError

    done dest pos -- TODO: pos, pos-1??? XXX

    where
      len = B.length bs

      f' dest m c = m >>= \s -> f dest s c
      {-# INLINE f' #-}

      -- No pending state.
      f    _ (pos, StateNone) 92 = return (pos, StateBackslash)
      f dest (pos, StateNone) c  = do
        A.unsafeWrite dest pos c
        return (pos + 1, decode UtfGround c)

      -- In the middle of parsing a UTF string.
      f dest (pos, StateUtf st) c = do
        A.unsafeWrite dest pos c
        return (pos + 1, decode st c)

      -- In the middle of escaping a backslash.
      f dest (pos, StateBackslash)  34 = writeAndReturn dest pos '\34' StateNone -- "
      f dest (pos, StateBackslash)  92 = writeAndReturn dest pos '\92' StateNone -- Backslash
      f dest (pos, StateBackslash)  47 = writeAndReturn dest pos '\47' StateNone -- /
      f dest (pos, StateBackslash)  98 = writeAndReturn dest pos  '\8' StateNone -- b
      f dest (pos, StateBackslash) 102 = writeAndReturn dest pos '\12' StateNone -- f
      f dest (pos, StateBackslash) 110 = writeAndReturn dest pos '\10' StateNone -- n
      f dest (pos, StateBackslash) 114 = writeAndReturn dest pos '\13' StateNone -- r
      f dest (pos, StateBackslash) 116 = writeAndReturn dest pos  '\9' StateNone -- t
      f    _ (pos, StateBackslash) 117 = return (pos, StateU0)                -- u
      f    _ (  _, StateBackslash) _   = throwDecodeError

      -- Processing '\u'.
      f _ (pos, StateU0) c =
        let w = decodeHex c in
        return (pos, StateU1 (w `shiftL` 12))

      f _ (pos, StateU1 w') c =
        let w = decodeHex c in
        return (pos, StateU2 (w' .|. (w `shiftL` 8)))

      f _ (pos, StateU2 w') c =
        let w = decodeHex c in
        return (pos, StateU3 (w' .|. (w `shiftL` 4)))

      f dest (pos, StateU3 w') c =
        let w = decodeHex c in
        let u = w' .|. w in

        -- Get next state based on surrogates.
        let st
              | u >= 0xd800 && u <= 0xdbff = -- High surrogate.
                StateS0 u
              | u >= 0xdc00 && u <= 0xdfff = -- Low surrogate.
                throwDecodeError
              | otherwise =
                StateNone
        in
        writeAndReturn dest pos (C.unsafeChr u) st

      -- Handle surrogates.
      f _ (pos, StateS0 u) 92 = return (pos, StateS1 u) -- Backslash
      f _ (  _, StateS0 _)  _ = throwDecodeError

      f _ (pos, StateS1 u) 117 = return (pos, StateSU0 u) -- u
      f _ (  _, StateS1 _)   _ = throwDecodeError

      f _ (pos, StateSU0 u) c =
        let w = decodeHex c in
        return (pos, StateSU1 u (w `shiftL` 12))

      f _ (pos, StateSU1 u w') c =
        let w = decodeHex c in
        return (pos, StateSU2 u (w' .|. (w `shiftL` 8)))

      f _ (pos, StateSU2 u w') c =
        let w = decodeHex c in
        return (pos, StateSU3 u (w' .|. (w `shiftL` 4)))

      f dest (pos, StateSU3 u w') c =
        let w = decodeHex c in
        let u2 = w' .|. w in

        -- Check if not low surrogate.
        if u2 < 0xdc00 || u2 > 0xdfff then
          throwDecodeError
        else
          writeAndReturn dest pos (U16.chr2 u u2) StateNone

      {-# INLINE f #-}

writeAndReturn :: A.MArray s -> Int -> Char -> t -> ST s (Int, t)
writeAndReturn dest pos char res = do
    i <- C.unsafeWrite dest pos char
    return (pos + i, res)
{-# INLINE writeAndReturn #-}

throwDecodeError :: a
throwDecodeError =
    let desc = "Data.Text.Internal.Encoding.decodeUtf8: Invalid UTF-8 stream" in
    throw (DecodeError desc Nothing)
{-# INLINE throwDecodeError #-}

unescapeText :: ByteString -> Either UnicodeException Text
unescapeText = unsafeDupablePerformIO . try . evaluate . unescapeText'
{-# INLINE unescapeText #-}
