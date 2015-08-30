module KTypes where

import qualified Data.Int as I
import qualified Data.Word as W
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U

data KObject =
    Atom KAtom
  | Vector KVector
  | Dictionary KDictionary
  | Table KTable
  | KeyTable KKeyTable
  | Function KFunction
  | Error KSymbol
  deriving Show

data KAtom =
    Boolean   KBoolean
  | Guid      KGuid
  | Byte      KByte
  | Short     KShort
  | Int       KInt
  | Long      KLong
  | Real      KReal
  | Float     KFloat
  | Char      KChar
  | Symbol    KSymbol
  | Timestamp KTimestamp
  | Month     KMonth
  | Date      KDate
  | DateTime  KDateTime
  | Timespan  KTimespan
  | Minute    KMinute
  | Second    KSecond
  | Time      KTime
  deriving Show

data KVector =
    ListV       KList
  | BooleanV   (U.Vector KBoolean)
  | GuidV      (V.Vector KGuid)
  | ByteV      (U.Vector KByte)
  | ShortV     (U.Vector KShort)
  | IntV       (U.Vector KInt)
  | LongV      (U.Vector KLong)
  | RealV      (U.Vector KReal)
  | FloatV     (U.Vector KFloat)
  | CharV      (U.Vector KChar)
  | SymbolV    (V.Vector KSymbol)
  | TimestampV (U.Vector KTimestamp)
  | MonthV     (U.Vector KMonth)
  | DateV      (U.Vector KDate)
  | DateTimeV  (U.Vector KDateTime)
  | TimespanV  (U.Vector KTimespan)
  | MinuteV    (U.Vector KMinute)
  | SecondV    (U.Vector KSecond)
  | TimeV      (U.Vector KTime)
  deriving Show

data KFunction =
    Lambda       KLambda
  | PrimVerb     KPrimVerb
  | Adverb       KAdverb
  | Projection   KProjection
  | Composition  KComposition
  deriving Show

type KBoolean     = Bool
type KGuid        = BS.ByteString
type KByte        = I.Int8
type KShort       = I.Int16
type KInt         = I.Int32
type KLong        = I.Int64
type KReal        = Float
type KFloat       = Double
type KChar        = W.Word8
type KSymbol      = BS.ByteString
type KTimestamp   = I.Int64
type KMonth       = I.Int32
type KDate        = I.Int32
type KDateTime    = Double
type KTimespan    = I.Int64
type KMinute      = I.Int32
type KSecond      = I.Int32
type KTime        = I.Int32
type KList        = V.Vector KObject
type KLambda      = (KSymbol, String)
type KPrimVerb    = (I.Int8, I.Int8)
type KAdverb      = (I.Int8, KFunction)
type KProjection  = V.Vector KObject
type KComposition = V.Vector KObject

type KDictionary  = (KVector, KVector)
type KTable       = (KVector, KList)
type KKeyTable    = (KTable,  KTable)

