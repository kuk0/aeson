{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}

-- |
-- Module:      Data.Aeson.Encoding.Builder
-- Copyright:   (c) 2011 MailRank, Inc.
--              (c) 2013 Simon Meier <iridcode@gmail.com>
-- License:     BSD3
-- Maintainer:  Bryan O'Sullivan <bos@serpentine.com>
-- Stability:   experimental
-- Portability: portable
--
-- Efficiently serialize a JSON value using the UTF-8 encoding.

module Data.Aeson.Encoding.Builder
    (
      encodeToBuilder
    , null_
    , bool
    , array
    , emptyArray_
    , emptyObject_
    , object
    , text
    , string
    , unquoted
    , quote
    , scientific
    , day
    , localTime
    , utcTime
    , timeOfDay
    , zonedTime
    , ascii2
    , ascii4
    , ascii5
    ) where

import Prelude ()
import Prelude.Compat

import Data.Aeson.Internal.Time
import Data.Aeson.Types.Internal (Value (..))
import Data.ByteString.Builder as B
import Data.ByteString.Builder.Prim as BP
import Data.ByteString.Builder.Scientific (scientificBuilder)
import Data.Char (chr, ord)
import Data.Monoid ((<>))
import Data.Scientific (Scientific, base10Exponent, coefficient)
import Data.Time (UTCTime(..))
import Data.Time.Calendar (Day(..), toGregorian)
import Data.Time.LocalTime
import Data.Word (Word8, Word64)
import qualified Data.HashMap.Strict as HMS
import qualified Data.Text as T
import qualified Data.Vector as V

import Data.Bits hiding (shiftR, shiftL)
import Data.Text.Internal (Text(..))
import Data.Text.Internal.Unsafe.Shift (shiftR, shiftL)
import Foreign.Ptr (minusPtr, plusPtr, castPtr)
import Foreign.Storable (poke)
import qualified Data.ByteString.Builder.Internal as B
import qualified Data.ByteString.Builder.Prim.Internal as BP
import qualified Data.Text.Array as A

-- | Encode a JSON value to a "Data.ByteString" 'B.Builder'.
--
-- Use this function if you are encoding over the wire, or need to
-- prepend or append further bytes to the encoded JSON value.
encodeToBuilder :: Value -> Builder
encodeToBuilder Null       = null_
encodeToBuilder (Bool b)   = bool b
encodeToBuilder (Number n) = scientific n
encodeToBuilder (String s) = text s
encodeToBuilder (Array v)  = array v
encodeToBuilder (Object m) = object m

-- | Encode a JSON null.
null_ :: Builder
null_ = BP.primBounded (ascii4 ('n',('u',('l','l')))) ()

-- | Encode a JSON boolean.
bool :: Bool -> Builder
bool = BP.primBounded (BP.condB id (ascii4 ('t',('r',('u','e'))))
                                   (ascii5 ('f',('a',('l',('s','e'))))))

-- | Encode a JSON array.
array :: V.Vector Value -> Builder
array v
  | V.null v  = emptyArray_
  | otherwise = B.char8 '[' <>
                encodeToBuilder (V.unsafeHead v) <>
                V.foldr withComma (B.char8 ']') (V.unsafeTail v)
  where
    withComma a z = B.char8 ',' <> encodeToBuilder a <> z

-- Encode a JSON object.
object :: HMS.HashMap T.Text Value -> Builder
object m = case HMS.toList m of
    (x:xs) -> B.char8 '{' <> one x <> foldr withComma (B.char8 '}') xs
    _      -> emptyObject_
  where
    withComma a z = B.char8 ',' <> one a <> z
    one (k,v)     = text k <> B.char8 ':' <> encodeToBuilder v

-- | Encode a JSON string.
text :: T.Text -> Builder
text t = B.char8 '"' <> unquoted t <> B.char8 '"'

-- | Encode a JSON string, without enclosing quotes.
unquoted :: T.Text -> Builder
unquoted = encodeUtf8BuilderEscaped escapeAscii

-- | Add quotes surrounding a builder
quote :: Builder -> Builder
quote b = B.char8 '"' <> b <> B.char8 '"'

-- | Encode a JSON string.
string :: String -> Builder
string t = B.char8 '"' <> BP.primMapListBounded go t <> B.char8 '"'
  where go = BP.condB (> '\x7f') BP.charUtf8 (c2w >$< escapeAscii)

escapeAscii :: BP.BoundedPrim Word8
escapeAscii =
    BP.condB (== c2w '\\'  ) (ascii2 ('\\','\\')) $
    BP.condB (== c2w '\"'  ) (ascii2 ('\\','"' )) $
    BP.condB (>= c2w '\x20') (BP.liftFixedToBounded BP.word8) $
    BP.condB (== c2w '\n'  ) (ascii2 ('\\','n' )) $
    BP.condB (== c2w '\r'  ) (ascii2 ('\\','r' )) $
    BP.condB (== c2w '\t'  ) (ascii2 ('\\','t' )) $
    BP.liftFixedToBounded hexEscape -- fallback for chars < 0x20
  where
    hexEscape :: BP.FixedPrim Word8
    hexEscape = (\c -> ('\\', ('u', fromIntegral c))) BP.>$<
        BP.char8 >*< BP.char8 >*< BP.word16HexFixed
{-# INLINE escapeAscii #-}

c2w :: Char -> Word8
c2w c = fromIntegral (ord c)

-- | Encode a JSON number.
scientific :: Scientific -> Builder
scientific s
    | e < 0     = scientificBuilder s
    | otherwise = B.integerDec (coefficient s * 10 ^ e)
  where
    e = base10Exponent s

emptyArray_ :: Builder
emptyArray_ = BP.primBounded (ascii2 ('[',']')) ()

emptyObject_ :: Builder
emptyObject_ = BP.primBounded (ascii2 ('{','}')) ()

ascii2 :: (Char, Char) -> BP.BoundedPrim a
ascii2 cs = BP.liftFixedToBounded $ const cs BP.>$< BP.char7 >*< BP.char7
{-# INLINE ascii2 #-}

ascii4 :: (Char, (Char, (Char, Char))) -> BP.BoundedPrim a
ascii4 cs = BP.liftFixedToBounded $ const cs >$<
    BP.char7 >*< BP.char7 >*< BP.char7 >*< BP.char7
{-# INLINE ascii4 #-}

ascii5 :: (Char, (Char, (Char, (Char, Char)))) -> BP.BoundedPrim a
ascii5 cs = BP.liftFixedToBounded $ const cs >$<
    BP.char7 >*< BP.char7 >*< BP.char7 >*< BP.char7 >*< BP.char7
{-# INLINE ascii5 #-}

ascii6 :: (Char, (Char, (Char, (Char, (Char, Char))))) -> BP.BoundedPrim a
ascii6 cs = BP.liftFixedToBounded $ const cs >$<
    BP.char7 >*< BP.char7 >*< BP.char7 >*< BP.char7 >*< BP.char7 >*< BP.char7
{-# INLINE ascii6 #-}

ascii8 :: (Char, (Char, (Char, (Char, (Char, (Char, (Char, Char)))))))
       -> BP.BoundedPrim a
ascii8 cs = BP.liftFixedToBounded $ const cs >$<
    BP.char7 >*< BP.char7 >*< BP.char7 >*< BP.char7 >*<
    BP.char7 >*< BP.char7 >*< BP.char7 >*< BP.char7
{-# INLINE ascii8 #-}

day :: Day -> Builder
day dd = encodeYear yr <>
         BP.primBounded (ascii6 ('-',(mh,(ml,('-',(dh,dl)))))) ()
  where (yr,m,d)    = toGregorian dd
        !(T mh ml)  = twoDigits m
        !(T dh dl)  = twoDigits d
        encodeYear y
            | y >= 1000 = B.integerDec y
            | y >= 0    = BP.primBounded (ascii4 (padYear y)) ()
            | y >= -999 = BP.primBounded (ascii5 ('-',padYear (- y))) ()
            | otherwise = B.integerDec y
        padYear y =
            let (ab,c) = fromIntegral y `quotRem` 10
                (a,b)  = ab `quotRem` 10
            in ('0',(digit a,(digit b,digit c)))
{-# INLINE day #-}

timeOfDay :: TimeOfDay -> Builder
timeOfDay t = timeOfDay64 (toTimeOfDay64 t)
{-# INLINE timeOfDay #-}

timeOfDay64 :: TimeOfDay64 -> Builder
timeOfDay64 (TOD h m s)
  | frac == 0 = hhmmss -- omit subseconds if 0
  | otherwise = hhmmss <> BP.primBounded showFrac frac
  where
    hhmmss  = BP.primBounded (ascii8 (hh,(hl,(':',(mh,(ml,(':',(sh,sl)))))))) ()
    !(T hh hl)  = twoDigits h
    !(T mh ml)  = twoDigits m
    !(T sh sl)  = twoDigits (fromIntegral real)
    (real,frac) = s `quotRem` pico
    showFrac = (\x -> ('.', x)) >$< (BP.liftFixedToBounded BP.char7 >*< trunc12)
    trunc12 = (`quotRem` micro) >$<
              BP.condB (\(_,y) -> y == 0) (fst >$< trunc6) (digits6 >*< trunc6)
    digits6 = ((`quotRem` milli) . fromIntegral) >$< (digits3 >*< digits3)
    trunc6  = ((`quotRem` milli) . fromIntegral) >$<
              BP.condB (\(_,y) -> y == 0) (fst >$< trunc3) (digits3 >*< trunc3)
    digits3 = (`quotRem` 10) >$< (digits2 >*< digits1)
    digits2 = (`quotRem` 10) >$< (digits1 >*< digits1)
    digits1 = BP.liftFixedToBounded (digit >$< BP.char7)
    trunc3  = BP.condB (== 0) BP.emptyB $
              (`quotRem` 100) >$< (digits1 >*< trunc2)
    trunc2  = BP.condB (== 0) BP.emptyB $
              (`quotRem` 10)  >$< (digits1 >*< trunc1)
    trunc1  = BP.condB (== 0) BP.emptyB digits1

    pico       = 1000000000000 -- number of picoseconds  in 1 second
    micro      =       1000000 -- number of microseconds in 1 second
    milli      =          1000 -- number of milliseconds in 1 second

timeZone :: TimeZone -> Builder
timeZone (TimeZone off _ _)
  | off == 0  = B.char7 'Z'
  | otherwise = BP.primBounded (ascii6 (s,(hh,(hl,(':',(mh,ml)))))) ()
  where !s         = if off < 0 then '-' else '+'
        !(T hh hl) = twoDigits h
        !(T mh ml) = twoDigits m
        (h,m)      = abs off `quotRem` 60
{-# INLINE timeZone #-}

dayTime :: Day -> TimeOfDay64 -> Builder
dayTime d t = day d <> B.char7 'T' <> timeOfDay64 t
{-# INLINE dayTime #-}

utcTime :: UTCTime -> B.Builder
utcTime (UTCTime d s) = dayTime d (diffTimeOfDay64 s) <> B.char7 'Z'
{-# INLINE utcTime #-}

localTime :: LocalTime -> Builder
localTime (LocalTime d t) = dayTime d (toTimeOfDay64 t)
{-# INLINE localTime #-}

zonedTime :: ZonedTime -> Builder
zonedTime (ZonedTime t z) = localTime t <> timeZone z
{-# INLINE zonedTime #-}

data T = T {-# UNPACK #-} !Char {-# UNPACK #-} !Char

twoDigits :: Int -> T
twoDigits a     = T (digit hi) (digit lo)
  where (hi,lo) = a `quotRem` 10

digit :: Int -> Char
digit x = chr (x + 48)

notEscaped :: Word64 -> Bool
notEscaped x = hasless x 0x20 == 0
            && hasvalue x 92 == 0 -- \
            && hasvalue x 34 == 0 -- "
{-# INLINE notEscaped #-}

pat80 :: Word64
pat80 = 0x8080808080808080
{-# INLINE pat80 #-}

pat01 :: Word64
pat01 = 0x0101010101010101
{-# INLINE pat01 #-}

pat :: Word64 -> Word64
pat n = pat01 * n
{-# INLINE pat #-}

hasless :: Word64 -> Word64 -> Word64
hasless x n = ((x - pat n) .&. complement x) .&. pat80
{-# INLINE hasless #-}

haszero :: Word64 -> Word64
haszero x = hasless x 1
{-# INLINE haszero #-}

hasvalue :: Word64 -> Word64 -> Word64
hasvalue x n = haszero $ x `xor` (pat n)
{-# INLINE hasvalue #-}

-- | Encode text using UTF-8 encoding and escape the ASCII characters using
-- a 'BP.BoundedPrim'.
--
-- Use this function is to implement efficient encoders for text-based formats
-- like JSON or HTML.
{-# INLINE encodeUtf8BuilderEscaped #-}
-- TODO: Extend documentation with references to source code in @blaze-html@
-- or @aeson@ that uses this function.
encodeUtf8BuilderEscaped :: BP.BoundedPrim Word8 -> Text -> B.Builder
encodeUtf8BuilderEscaped be =
    -- manual eta-expansion to ensure inlining works as expected
    \txt -> B.builder (mkBuildstep txt)
  where
    bound = max 4 $ BP.sizeBound be

    mkBuildstep (Text arr off len) !k =
        outerLoop off
      where
        iend = off + len

        outerLoop !i0 br@(B.BufferRange op0 ope)
          | i0 >= iend       = k br
          | outRemaining > 0 = goPartial iendTmp (iendTmp `shiftR` 3)
          -- TODO: Use a loop with an integrated bound's check if outRemaining
          -- is smaller than 8, as this will save on divisions.
          | otherwise        = return $ B.bufferFull bound op0 (outerLoop i0)
          where
            outRemaining = (ope `minusPtr` op0) `div` bound
            inpRemaining = iend - i0
            iendTmp = (i0 + min outRemaining inpRemaining)

            goPartial !iendTmp !iendTmp64 = go i0 op0
              where
                go64 !j !op
                  | j < iendTmp64 && w .&. pat80 == 0 && notEscaped w = do
                      poke64 w
                      go64 (j+1) (op `plusPtr` 8)
                  | otherwise = go (j `shiftL` 3) (castPtr op)
                  where
                    poke64 v = poke op (v :: Word64)
                    w = A.unsafeIndex64 arr j

                go !i !op
                  | i .&. 7 == 0 && i + 7 < iendTmp && w .&. pat80 == 0 && notEscaped w = do
                      poke64 w
                      go64 ((i `shiftR` 3) + 1) (castPtr $ op `plusPtr` 8)
                  | i < iendTmp = case a of
                      a | a <= 0x7F ->
                          BP.runB be (fromIntegral a) op >>= go (i + 1)
                        | 0xC2 <= a && a <= 0xDF -> do
                            poke8 0 a
                            poke8 1 b
                            go (i + 2) (op `plusPtr` 2)
                        | 0xE0 <= a && a <= 0xEF -> do
                            poke8 0 a
                            poke8 1 b
                            poke8 2 c
                            go (i + 3) (op `plusPtr` 3)
                        | otherwise -> do
                            poke8 0 a
                            poke8 1 b
                            poke8 2 c
                            poke8 3 d
                            go (i + 4) (op `plusPtr` 4)
                  | otherwise =
                      outerLoop i (B.BufferRange op ope)
                  where
                    poke64 v = poke (castPtr op) (v :: Word64)
                    poke8 j v = poke (op `plusPtr` j) (v :: Word8)
                    w = A.unsafeIndex64 arr (i `shiftR` 3)
                    a = A.unsafeIndex arr i
                    b = A.unsafeIndex arr (i+1)
                    c = A.unsafeIndex arr (i+2)
                    d = A.unsafeIndex arr (i+3)
