module Passwords where

import Data.Bits
import Data.ByteString as BS
import Data.ByteString.UTF8 as UTF8
import Data.Digest.SHA1 as SHA1
import Foreign

import Database
import SQLite3
import Types


hashPassword :: String -> BS.ByteString
hashPassword cleartext
    = word160ToByteString $ SHA1.hash $ BS.unpack $ UTF8.fromString cleartext

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

validatePassword :: Int64 -> String -> Buglist Bool
validatePassword userID cleartext = do
  hashed <- return $ hashPassword cleartext
  [[SQLInteger valid]] <- query "SELECT password_hash = ? FROM users WHERE id = ?"
                                [SQLBlob hashed, SQLInteger userID]
  return $ case valid of
    0 -> False
    _ -> True
