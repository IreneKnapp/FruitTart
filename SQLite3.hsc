{-# LANGUAGE ForeignFunctionInterface #-}
module SQLite3 (
                Database,
                Statement,
                Error(..),
                StepResult(..),
                open,
                close,
                prepare,
                step,
                reset,
                finalize,
                bindBlob,
                bindDouble,
                bindInt,
                bindInt64,
                bindNull,
                bindText
               )
    where

import qualified Data.ByteString as BS
import qualified Data.ByteString.UTF8 as UTF8
import Foreign
import Foreign.C

#include <sqlite3.h>

newtype Database = Database (Ptr ())
newtype Statement = Statement (Ptr ())

data Error = ErrorOK
           | ErrorError
           | ErrorInternal
           | ErrorPermission
           | ErrorAbort
           | ErrorBusy
           | ErrorLocked
           | ErrorNoMemory
           | ErrorReadOnly
           | ErrorInterrupt
           | ErrorIO
           | ErrorNotFound
           | ErrorCorrupt
           | ErrorFull
           | ErrorCan'tOpen
           | ErrorProtocol
           | ErrorEmpty
           | ErrorSchema
           | ErrorTooBig
           | ErrorConstraint
           | ErrorMismatch
           | ErrorMisuse
           | ErrorNoLargeFileSupport
           | ErrorAuthorization
           | ErrorFormat
           | ErrorRange
           | ErrorNotADatabase
           | ErrorRow
           | ErrorDone
             deriving (Eq, Show)

data StepResult = Row | Done deriving (Eq, Show)


encodeError :: Error -> Int
encodeError ErrorOK = 0
encodeError ErrorError = 1
encodeError ErrorInternal = 2
encodeError ErrorPermission = 3
encodeError ErrorAbort = 4
encodeError ErrorBusy = 5
encodeError ErrorLocked = 6
encodeError ErrorNoMemory = 7
encodeError ErrorReadOnly = 8
encodeError ErrorInterrupt = 9
encodeError ErrorIO = 10
encodeError ErrorNotFound = 11
encodeError ErrorCorrupt = 12
encodeError ErrorFull = 13
encodeError ErrorCan'tOpen = 14
encodeError ErrorProtocol = 15
encodeError ErrorEmpty = 16
encodeError ErrorSchema = 17
encodeError ErrorTooBig = 18
encodeError ErrorConstraint = 19
encodeError ErrorMismatch = 20
encodeError ErrorMisuse = 21
encodeError ErrorNoLargeFileSupport = 22
encodeError ErrorAuthorization = 23
encodeError ErrorFormat = 24
encodeError ErrorRange = 25
encodeError ErrorNotADatabase = 26
encodeError ErrorRow = 100
encodeError ErrorDone = 101


decodeError :: Int -> Error
decodeError 0 = ErrorOK
decodeError 1 = ErrorError
decodeError 2 = ErrorInternal
decodeError 3 = ErrorPermission
decodeError 4 = ErrorAbort
decodeError 5 = ErrorBusy
decodeError 6 = ErrorLocked
decodeError 7 = ErrorNoMemory
decodeError 8 = ErrorReadOnly
decodeError 9 = ErrorInterrupt
decodeError 10 = ErrorIO
decodeError 11 = ErrorNotFound
decodeError 12 = ErrorCorrupt
decodeError 13 = ErrorFull
decodeError 14 = ErrorCan'tOpen
decodeError 15 = ErrorProtocol
decodeError 16 = ErrorEmpty
decodeError 17 = ErrorSchema
decodeError 18 = ErrorTooBig
decodeError 19 = ErrorConstraint
decodeError 20 = ErrorMismatch
decodeError 21 = ErrorMisuse
decodeError 22 = ErrorNoLargeFileSupport
decodeError 23 = ErrorAuthorization
decodeError 24 = ErrorFormat
decodeError 25 = ErrorRange
decodeError 26 = ErrorNotADatabase
decodeError 100 = ErrorRow
decodeError 101 = ErrorDone


sqlError :: String -> Error -> IO a
sqlError functionName error = do
  fail $ "SQLite3 returned " ++ (show error)
         ++ " while attempting to perform " ++ functionName
         ++ "."

foreign import ccall "sqlite3_open"
  open' :: CString -> Ptr (Ptr ()) -> IO Int
openError :: String -> IO (Either Database Error)
openError path = do
  BS.useAsCString (UTF8.fromString path)
                  (\path -> do
                     alloca (\database -> do
                               error <- open' path database
                               error <- return $ decodeError error
                               case error of
                                 ErrorOK -> do
                                            database <- peek database
                                            return $ Left $ Database database
                                 _ -> return $ Right error))
open :: String -> IO Database
open path = do
  databaseOrError <- openError path
  case databaseOrError of
    Left database -> return database
    Right error -> sqlError "open" error

foreign import ccall "sqlite3_close"
  close' :: Ptr () -> IO Int
closeError :: Database -> IO Error
closeError (Database database) = do
  error <- close' database
  return $ decodeError error
close :: Database -> IO ()
close database = do
  error <- closeError database
  case error of
    ErrorOK -> return ()
    _ -> sqlError "close" error

foreign import ccall "sqlite3_prepare_v2"
  prepare' :: Ptr () -> CString -> Int -> Ptr (Ptr ()) -> Ptr (Ptr ()) -> IO Int
prepareError :: Database -> String -> IO (Either Statement Error)
prepareError (Database database) text = do
  BS.useAsCString (UTF8.fromString text)
                  (\text -> do
                     alloca (\statement -> do
                               error <- prepare' database text (-1) statement nullPtr
                               error <- return $ decodeError error
                               case error of
                                 ErrorOK -> do
                                            statement <- peek statement
                                            return $ Left $ Statement statement
                                 _ -> return $ Right error))
prepare :: Database -> String -> IO Statement
prepare database text = do
  statementOrError <- prepareError database text
  case statementOrError of
    Left statement -> return statement
    Right error -> sqlError "prepare" error

foreign import ccall "sqlite3_step"
  step' :: Ptr () -> IO Int
stepError :: Statement -> IO Error
stepError (Statement statement) = do
  error <- step' statement
  return $ decodeError error
step :: Statement -> IO StepResult
step statement = do
  error <- stepError statement
  case error of
    ErrorRow -> return Row
    ErrorDone -> return Done
    _ -> sqlError "step" error

foreign import ccall "sqlite3_reset"
  reset' :: Ptr () -> IO Int
resetError :: Statement -> IO Error
resetError (Statement statement) = do
  error <- reset' statement
  return $ decodeError error
reset :: Statement -> IO ()
reset statement = do
  error <- resetError statement
  case error of
    ErrorOK -> return ()
    _ -> sqlError "reset" error

foreign import ccall "sqlite3_finalize"
  finalize' :: Ptr () -> IO Int
finalizeError :: Statement -> IO Error
finalizeError (Statement statement) = do
  error <- finalize' statement
  return $ decodeError error
finalize :: Statement -> IO ()
finalize statement = do
  error <- finalizeError statement
  case error of
    ErrorOK -> return ()
    _ -> sqlError "finalize" error

foreign import ccall "sqlite3_bind_blob"
  bindBlob' :: Ptr () -> Int -> Ptr () -> Int -> Ptr () -> IO Int
bindBlobError :: Statement -> Int -> BS.ByteString -> IO Error
bindBlobError (Statement statement) parameterIndex byteString = do
  size <- return $ BS.length byteString
  BS.useAsCString byteString
                  (\data' -> do
                     error <- bindBlob' statement parameterIndex (castPtr data') size
                                        (intPtrToPtr (-1))
                     return $ decodeError error)
bindBlob :: Statement -> Int -> BS.ByteString -> IO ()
bindBlob statement parameterIndex byteString = do
  error <- bindBlobError statement parameterIndex byteString
  case error of
    ErrorOK -> return ()
    _ -> sqlError "bind blob" error

foreign import ccall "sqlite3_bind_double"
  bindDouble' :: Ptr () -> Int -> Double -> IO Int
bindDoubleError :: Statement -> Int -> Double -> IO Error
bindDoubleError (Statement statement) parameterIndex datum = do
  error <- bindDouble' statement parameterIndex datum
  return $ decodeError error
bindDouble :: Statement -> Int -> Double -> IO ()
bindDouble statement parameterIndex datum = do
  error <- bindDoubleError statement parameterIndex datum
  case error of
    ErrorOK -> return ()
    _ -> sqlError "bind double" error

foreign import ccall "sqlite3_bind_int"
  bindInt' :: Ptr () -> Int -> Int -> IO Int
bindIntError :: Statement -> Int -> Int -> IO Error
bindIntError (Statement statement) parameterIndex datum = do
  error <- bindInt' statement parameterIndex datum
  return $ decodeError error
bindInt :: Statement -> Int -> Int -> IO ()
bindInt statement parameterIndex datum = do
  error <- bindIntError statement parameterIndex datum
  case error of
    ErrorOK -> return ()
    _ -> sqlError "bind int" error

foreign import ccall "sqlite3_bind_int64"
  bindInt64' :: Ptr () -> Int -> Int64 -> IO Int
bindInt64Error :: Statement -> Int -> Int64 -> IO Error
bindInt64Error (Statement statement) parameterIndex datum = do
  error <- bindInt64' statement parameterIndex datum
  return $ decodeError error
bindInt64 :: Statement -> Int -> Int64 -> IO ()
bindInt64 statement parameterIndex datum = do
  error <- bindInt64Error statement parameterIndex datum
  case error of
    ErrorOK -> return ()
    _ -> sqlError "bind int64" error

foreign import ccall "sqlite3_bind_null"
  bindNull' :: Ptr () -> Int -> IO Int
bindNullError :: Statement -> Int -> IO Error
bindNullError (Statement statement) parameterIndex = do
  error <- bindNull' statement parameterIndex
  return $ decodeError error
bindNull :: Statement -> Int -> IO ()
bindNull statement parameterIndex = do
  error <- bindNullError statement parameterIndex
  case error of
    ErrorOK -> return ()
    _ -> sqlError "bind null" error

foreign import ccall "sqlite3_bind_text"
  bindText' :: Ptr () -> Int -> CString -> Int -> Ptr () -> IO Int
bindTextError :: Statement -> Int -> String -> IO Error
bindTextError (Statement statement) parameterIndex text = do
  byteString <- return $ UTF8.fromString text
  size <- return $ BS.length byteString
  BS.useAsCString byteString
                  (\data' -> do
                     error <- bindText' statement parameterIndex data' size
                                        (intPtrToPtr (-1))
                     return $ decodeError error)
bindText :: Statement -> Int -> String -> IO ()
bindText statement parameterIndex text = do
  error <- bindTextError statement parameterIndex text
  case error of
    ErrorOK -> return ()
    _ -> sqlError "bind text" error
