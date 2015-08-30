
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

parseObject :: Bool -> Get.Get KObject
parseObject le = do
    typeCode <- Get.getWord8
    parseWithType (fromIntegral typeCode) le

parseWithType :: I.Int8 -> Bool -> Get.Get KObject
parseWithType tc le
  | tc >= -19 && tc < 0 = parseAtom   tc le >>= return . Atom
  | tc  <  19 && tc > 0 = parseVector tc le >>= return . Vector

parseAtom :: I.Int8 -> Bool -> Get.Get KAtom
parseAtom tc le = case fromIntegral tc of
    -19 -> getWord32     >>= return . Time . fromIntegral
    -18 -> getWord32     >>= return . Second . fromIntegral
    -17 -> getWord32     >>= return . Minute . fromIntegral
    -16 -> getWord64     >>= return . Timespan . fromIntegral
    -15 -> parseDbl      >>= return . DateTime 
    -14 -> getWord32     >>= return . Date . fromIntegral
    -13 -> getWord32     >>= return . Month . fromIntegral
    -12 -> getWord64     >>= return . Timestamp . fromIntegral
    -11 -> parseSym      >>= return . Symbol 
    -10 -> Get.getWord8  >>= return . Char . fromIntegral
    -9  -> parseDbl      >>= return . Float 
    -8  -> parseFlt      >>= return . Real 
    -7  -> getWord64     >>= return . Long . fromIntegral
    -6  -> getWord32     >>= return . Int . fromIntegral
    -5  -> getWord16     >>= return . Short . fromIntegral
    -4  -> Get.getWord8  >>= return . Byte . fromIntegral
    -2  -> Get.getByteString 128 >>= return . Guid 
    -1  -> Get.getWord8  >>= return . Boolean . not . (==0)
    _   -> undefined
    where
        getWord16 = if le then Get.getWord16le else Get.getWord16be
        getWord32 = if le then Get.getWord32le else Get.getWord32be
        getWord64 = if le then Get.getWord64le else Get.getWord64be
        parseDbl  = return . wordToDouble =<< getWord64
        parseFlt  = return . wordToFloat  =<< getWord32

-- TODO: find more efficient construction
parseSym :: Get.Get BS.ByteString
parseSym = go >>= return . BS.pack 
    where go = Get.getWord8 >>= 
               \b -> if b == 0 then return [] else go >>= return . (b:)

parseVector :: I.Int8 -> Bool -> Get.Get KVector
parseVector tc le = do
    Get.skip 2 -- skip the element type and attribute flags
    len <- Get.getWord32le
    parseVectorLen tc $ fromIntegral len

-- TODO: Make sure using Int for the length is not too much a limitation
parseVectorLen :: I.Int8 -> Int -> Get.Get KVector
parseVectorLen tc len
  -- | tc `elem` [-19, -18, -17, -14, -13, -6] = replicateM len Get.getWord32le >>= return . Vector . U.fromList
  | otherwise = undefined
