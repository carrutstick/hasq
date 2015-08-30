module Konnect where

import KTypes
import KParse

import GHC.IO.Handle (Handle)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Char8 as BSC
import qualified Network as N
import qualified Data.Word as W
import qualified Data.Binary.Get as G
import qualified Data.Binary.Put as P

import Debug.Trace (trace)

data Konnection = Konn { handle :: Handle
                       , creds  :: BS.ByteString
                       , ver    :: W.Word8
                       } deriving Show

toStrict = BS.concat . BSL.toChunks
toLazy = BSL.fromChunks . (:[])

konnect' :: N.HostName -> N.PortID -> String 
        -> IO (Either String Konnection)
konnect' host port cred = do
    let msg = BSC.pack $ cred ++ "\x01\x00"
    h <- N.connectTo host port
    BS.hPut h msg
    ret <- BS.hGet h 1
    if BS.null ret
    then return $ Left "Could not verify credentials"
    else return $ Right $ Konn { handle = h
                               , creds = (BSC.pack cred)
                               , ver = BS.head ret }

konnect :: N.HostName -> Int -> String -> IO Konnection
konnect hn p cred = do
    kon <- konnect' hn (N.PortNumber $ fromIntegral p) cred
    case kon of
        Left  s -> fail s
        Right k -> return k

query :: Konnection -> BS.ByteString -> IO ()
query kon qstr = do
    let h = handle kon
    let q = toStrict $ P.runPut $ buildQuery qstr
    BS.hPut h q

buildQuery :: BS.ByteString -> P.Put
buildQuery q = do
    let qlen = fromIntegral $ BS.length q
    let size = 8 + 6 + qlen
    -- Message header
    P.putWord8 1 -- Little endian
    P.putWord8 1 -- Synchronous
    P.putWord8 0 -- Compression
    P.putWord8 0 -- Unused
    P.putWord32le size
    -- Vector header
    P.putWord8 10 -- Char array
    P.putWord8 0  -- Vector attributes
    P.putWord32le qlen
    -- Vector itself
    P.putByteString q

parseMsg :: Konnection -> IO KObject
parseMsg kon = do
    let h = handle kon
    hdr <- BS.hGet h 4
    let littleEnd = 1 == BS.head hdr
    let getWord = if littleEnd then G.getWord32le else G.getWord32be
    len <- BS.hGet h 4 >>= 
           return . fromIntegral . (G.runGet getWord) . toLazy
    dat <- BS.hGet h (len - 8)
    return $ G.runGet (parseObject littleEnd) $ toLazy dat

getQuery :: Konnection -> BS.ByteString -> IO KObject
getQuery kon q = query kon q >> parseMsg kon

