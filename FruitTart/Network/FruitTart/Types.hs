{-# LANGUAGE TypeSynonymInstances, ExistentialQuantification,
             GADTs, EmptyDataDecls #-}
module Network.FruitTart.Types (
                                -- General
                                FruitTartState(..),
                                FruitTart,
                                
                                -- Custard language
                                CustardValueType(..),
                                CustardStringTypeWitness,
                                CustardLambdaTypeWitness,
                                CustardValue(..),
                                AnyCustardValue(..),
                                CustardParameter(..),
                                CustardExpression(..),
                                CustardContext(..),
                                CustardContextType(..),
                                CustardToken(..)
                               )
  where

import Control.Concurrent
import Control.Exception
import Control.Monad.State
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Dynamic
import Data.Int
import Data.List
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe

import Database.SQLite3
import Network.FastCGI


data FruitTartState  = FruitTartState {
      database :: Database,
      captchaCacheMVar :: MVar (Map Int64 (String, ByteString)),
      sessionID :: Maybe Int64
    }

type FruitTart = StateT FruitTartState FastCGI

instance MonadFastCGI FruitTart where
    getFastCGIState = lift getFastCGIState
    implementationThrowFastCGI exception = lift $ fThrow exception
    implementationCatchFastCGI action handler = do
      state <- get
      (result, state')
          <- lift $ fCatch (evalStateT (do
                                         result <- action
                                         state' <- get
                                         return (result, state'))
                                       state)
                           (\e -> evalStateT
                                    (do
                                      result <- handler $ fromJust $ fromException e
                                      state' <- get
                                      return (result, state'))
                                    state)
      put state'
      return result
    implementationBlockFastCGI action = do
      state <- get
      (result, state') <- lift $ fBlock (evalStateT (do
                                                      result <- action
                                                      state' <- get
                                                      return (result, state'))
                                                    state)
      put state'
      return result
    implementationUnblockFastCGI action = do
      state <- get
      (result, state') <- lift $ fUnblock (evalStateT (do
                                                        result <- action
                                                        state' <- get
                                                        return (result, state'))
                                                      state)
      put state'
      return result


data CustardValueType = CBool
                      | CInt
                      | CString
                      | CMaybeInt
                      | CMaybeString

data CustardParameter = CustardParameter (String, String)

data CustardStringTypeWitness
data CustardLambdaTypeWitness

data CustardValue t where
  CustardNull :: CustardValue ()
  CustardBool :: Bool -> CustardValue Bool
  CustardInteger :: Int64 -> CustardValue Int64
  CustardCharacter :: Char -> CustardValue Char
  CustardString :: String -> CustardValue CustardStringTypeWitness
  CustardList :: [CustardValue a] -> CustardValue [a]
  CustardMaybe :: (Maybe (CustardValue a)) -> CustardValue (Maybe a)
  CustardOrdering :: Ordering -> CustardValue Ordering
  CustardMap :: Map (String, String) AnyCustardValue
             -> CustardValue (Map (String, String) AnyCustardValue)
  CustardLambda :: [CustardParameter]
                -> Map (String, String) AnyCustardValue
                -> CustardExpression
                -> CustardValue CustardLambdaTypeWitness
  CustardNativeLambda :: (CustardContext
                          -> [CustardValue a]
                          -> FruitTart AnyCustardValue)
                      -> CustardValue CustardLambdaTypeWitness

data AnyCustardValue = forall a . SomeCustardValue (CustardValue a)

data CustardExpression = CustardStringLiteral (CustardValue CustardStringTypeWitness)
                        | CustardIntegerLiteral (CustardValue Int64)
                        | CustardExpressionList [CustardExpression]
                        | CustardOperationConcatenate CustardExpression
                                                       CustardExpression
                        | CustardOperationEquals CustardExpression
                                                  CustardExpression
                        | CustardOperationNotEquals CustardExpression
                                                     CustardExpression
                        | CustardOperationAnd CustardExpression CustardExpression
                        | CustardOperationOr CustardExpression CustardExpression
                        | CustardOperationNot CustardExpression
                        | CustardOperationGreater CustardExpression
                                                   CustardExpression
                        | CustardOperationGreaterEquals CustardExpression
                                                         CustardExpression
                        | CustardOperationLess CustardExpression
                                                CustardExpression
                        | CustardOperationLessEquals CustardExpression
                                                      CustardExpression
                        | CustardOperationAdd CustardExpression
                                              CustardExpression
                        | CustardOperationSubtract CustardExpression
                                                   CustardExpression
                        | CustardOperationMultiply CustardExpression
                                                   CustardExpression
                        | CustardOperationDivide CustardExpression
                                                 CustardExpression
                        | CustardIfExpression [CustardExpression]
                        | CustardCaseExpression [CustardExpression]
                        | CustardCallExpression [CustardExpression]
                        | CustardIterateExpression [CustardExpression]
                        | CustardQueryExpression [CustardExpression]
                        | CustardBoundExpression [CustardExpression]
                        | CustardFunctionCall CustardExpression
                                               [CustardExpression]
                        | CustardVariable (String, String)
                        | CustardLambdaExpression [CustardParameter]
                                                  CustardExpression
                        | CustardBindExpression [CustardExpression]
                        | CustardBindMapExpression [CustardExpression]
                        | CustardBindQuery1Expression [CustardExpression]
                        | CustardBindQueryNExpression [CustardExpression]
                        | CustardSequence CustardExpression CustardExpression

data CustardContext = CustardContext {
    custardContextType :: CustardContextType,
    custardContextFormInputMap :: Map String String,
    custardContextParameters :: [AnyCustardValue],
    custardContextBindings :: Map (String, String) AnyCustardValue
  }

data CustardContextType = ControllerContext | TemplateContext

data CustardToken = TokenStringValue (CustardValue CustardStringTypeWitness)
                   | TokenIntegerValue (CustardValue Int64)
                   | TokenSymbol String String
                   | TokenIf
                   | TokenCase
                   | TokenCall
                   | TokenIterate
                   | TokenQuery
                   | TokenBound
                   | TokenBind
                   | TokenBindMap
                   | TokenBindQuery1
                   | TokenBindQueryN
                   | TokenLeftParen
                   | TokenRightParen
                   | TokenLeftSquareBracket
                   | TokenRightSquareBracket
                   | TokenLeftCurlyBracket
                   | TokenRightCurlyBracket
                   | TokenMinusGreater
                   | TokenSemicolon
                   | TokenComma
                   | TokenPlusPlus
                   | TokenEqualsEquals
                   | TokenExclamationEquals
                   | TokenExclamation
                   | TokenAmpersandAmpersand
                   | TokenBarBar
                   | TokenGreaterEquals
                   | TokenGreater
                   | TokenLessEquals
                   | TokenLess
                   | TokenPlus
                   | TokenMinus
                   | TokenStar
                   | TokenSlash
