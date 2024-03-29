module Network.FruitTart.Custard.Functions.Passwords (
                                                      cfHashPassword
                                                     )
  where

import Control.Concurrent.MVar
import Control.Exception
import Control.Monad.State
import Data.Int
import Data.List
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import Prelude hiding (catch)

import Data.Bits
import Data.ByteString as BS
import Data.ByteString.UTF8 as UTF8
import Data.Digest.SHA1 as SHA1
import Foreign

import Network.FruitTart.Custard.Syntax
import Network.FruitTart.Custard.Functions.Util
import Network.FruitTart.Types


cfHashPassword :: CustardContext
               -> [CustardValue]
               -> FruitTart (CustardContext, CustardValue)
cfHashPassword context parameters = do
  requireNParameters parameters 1 "hashPassword"
  cleartext <- valueToUTF8String $ parameters !! 0
  return (context, CustardData $ hashPassword cleartext)


hashPassword :: BS.ByteString -> BS.ByteString
hashPassword cleartext
    = word160ToByteString $ SHA1.hash $ BS.unpack cleartext

word160ToByteString :: SHA1.Word160 -> BS.ByteString
word160ToByteString (SHA1.Word160 a b c d e) = 
    BS.concat [word32ToByteString a,
               word32ToByteString b,
               word32ToByteString c,
               word32ToByteString d,
               word32ToByteString e]

word32ToByteString :: Word32 -> BS.ByteString
word32ToByteString word = BS.pack [fromIntegral $ (word `shiftR` 24) .&. 0xFF,
                                   fromIntegral $ (word `shiftR` 16) .&. 0xFF,
                                   fromIntegral $ (word `shiftR` 8) .&. 0xFF,
                                   fromIntegral $ (word `shiftR` 0) .&. 0xFF]
