
module KParse where

import KTypes
import qualified Data.Binary.Get as Get
import qualified Data.ByteString as BS
import qualified Data.Int as I
import qualified Data.Word as W
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U
import Data.Binary.IEEE754 (floatToWord, wordToFloat, doubleToWord, wordToDouble)
import Control.Monad (replicateM)

import Debug.Trace (trace)

parseObject :: Bool -> Get.Get KObject
parseObject le = do
    typeCode <- Get.getWord8
    parseWithType (fromIntegral typeCode) le

parseWithType :: I.Int8 -> Bool -> Get.Get KObject
parseWithType tc le
  | tc >= -19 && tc <  0 = fmap Atom   $ parseAtom   tc le
  | tc  <  19 && tc >= 0 = fmap Vector $ parseVector tc le
  | otherwise            = return $ Error "Unknown type"

parseAtom :: I.Int8 -> Bool -> Get.Get KAtom
parseAtom tc le = case fromIntegral tc of
    -19 -> fmap (Time . fromIntegral) getWord32 
    -18 -> fmap (Second . fromIntegral) getWord32
    -17 -> fmap (Minute . fromIntegral) getWord32
    -16 -> fmap (Timespan . fromIntegral) getWord64
    -15 -> fmap DateTime parseDbl
    -14 -> fmap (Date . fromIntegral) getWord32
    -13 -> fmap (Month . fromIntegral) getWord32
    -12 -> fmap (Timestamp . fromIntegral) getWord64
    -11 -> fmap Symbol parseSym
    -10 -> fmap (Char . fromIntegral) Get.getWord8
    -9  -> fmap Float parseDbl
    -8  -> fmap Real parseFlt
    -7  -> fmap (Long . fromIntegral) getWord64
    -6  -> fmap (Int . fromIntegral) getWord32
    -5  -> fmap (Short . fromIntegral) getWord16
    -4  -> fmap (Byte . fromIntegral) Get.getWord8
    -2  -> fmap Guid (Get.getByteString 128)
    -1  -> fmap (Boolean . not . (==0)) Get.getWord8
    _   -> undefined
    where
        getWord16 = if le then Get.getWord16le else Get.getWord16be
        getWord32 = if le then Get.getWord32le else Get.getWord32be
        getWord64 = if le then Get.getWord64le else Get.getWord64be
        parseDbl  = wordToDouble `fmap` getWord64
        parseFlt  = wordToFloat  `fmap` getWord32

parseVector :: I.Int8 -> Bool -> Get.Get KVector
parseVector tc le = do
    attr <- Get.getWord8
    len <- getWord32
    parseVectorLen tc le len
    where getWord32 = if le then Get.getWord32le else Get.getWord32be

parseVectorLen :: I.Int8 -> Bool -> W.Word32 -> Get.Get KVector
parseVectorLen tc le len = case tc of
    19 -> fmap TimeV      (ui getWord32)
    18 -> fmap SecondV    (ui getWord32)
    17 -> fmap MinuteV    (ui getWord32)
    16 -> fmap TimespanV  (ui getWord64)
    15 -> fmap DateTimeV  (u  parseDbl)
    14 -> fmap DateV      (ui getWord32)
    13 -> fmap MonthV     (ui getWord32)
    12 -> fmap TimestampV (ui getWord64)
    11 -> fmap SymbolV    (v  parseSym)
    10 -> fmap CharV      (ui Get.getWord8)
    9  -> fmap FloatV     (u  parseDbl)
    8  -> fmap RealV      (u  parseFlt)
    7  -> fmap LongV      (ui getWord64)
    6  -> fmap IntV       (ui getWord32)
    5  -> fmap ShortV     (ui getWord16)
    4  -> fmap ByteV      (ui Get.getWord8)
    2  -> fmap GuidV      (v $ Get.getByteString 128)
    1  -> fmap BooleanV   (u  (fmap (not . (==0)) Get.getWord8))
    0  -> fmap ListV      (v $ parseObject le)
    _  -> undefined
    where
        getWord16 = if le then Get.getWord16le else Get.getWord16be
        getWord32 = if le then Get.getWord32le else Get.getWord32be
        getWord64 = if le then Get.getWord64le else Get.getWord64be
        parseDbl  = return . wordToDouble =<< getWord64
        parseFlt  = return . wordToFloat  =<< getWord32

        v :: Get.Get a -> Get.Get (V.Vector a)
        v = V.replicateM (fromIntegral len)

        u :: U.Unbox a => Get.Get a -> Get.Get (U.Vector a)
        u = U.replicateM (fromIntegral len)

        ui :: (Integral b, Num a, U.Unbox a, U.Unbox b)
           => Get.Get b -> Get.Get (U.Vector a)
        ui = fmap (U.map fromIntegral) . u

-- TODO: find more efficient construction
parseSym :: Get.Get BS.ByteString
parseSym = go >>= return . BS.pack 
    where go = Get.getWord8 >>= 
               \b -> if b == 0 then return [] else go >>= return . (b:)

