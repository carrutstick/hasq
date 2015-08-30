module Main where

import Hasq
import qualified Data.ByteString as BS

main :: IO ()
main = do
    kon <- konnect "localhost" 5555 ""
    getQuery kon "`hi" >>= putStrLn . show
