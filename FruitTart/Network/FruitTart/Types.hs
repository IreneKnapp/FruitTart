{-# LANGUAGE TypeSynonymInstances, FlexibleInstances,
             ExistentialQuantification #-}
module Network.FruitTart.Types (
                                -- General
                                FruitTartState(..),
                                FruitTart,
                                
                                -- Design
                                Design(..),
                                Module(..),
                                Controller(..),
                                Function(..),
                                Query(..),
                                Template(..),
                                ParameterType(..),
                                
                                -- Custard language
                                CustardValueType(..),
                                CustardValue(..),
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
import Data.Set (Set)
import qualified Data.Set as Set

import Network.Socket.Internal (HostAddress)

import Database.SQLite3
import Network.FastCGI


data FruitTartState  = FruitTartState {
      database :: Database,
      designMVar :: MVar Design,
      captchaCacheMVar :: MVar (Map Int64 (String, ByteString)),
      sessionID :: Maybe Int64,
      maybeCurrentPage :: Maybe String,
      maybeControllerName :: Maybe String
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


data Design = Design {
    designModules :: Set Module,
    designControllerMappings :: Map String String,
    designControllers :: Map String Controller,
    designFunctions :: Map (String, String) Function,
    designQueries :: Map (String, String) Query,
    designTemplates :: Map (String, String) Template
  }


data Module = Module {
  }


data Controller = Controller {
  }


data Function = Function {
  }


data Query = Query {
  }


data Template = Template {
  }


data ParameterType = IntegerParameter
                   | StringParameter


data CustardValueType = CBool
                      | CInt
                      | CString
                      | CMaybeInt
                      | CMaybeString

data CustardParameter = CustardParameter (String, String)

data CustardValue = CustardNull
                  | CustardSymbol String String
                  | CustardBool Bool
                  | CustardInteger Int64
                  | CustardCharacter Char
                  | CustardString ByteString
                  | CustardList [CustardValue]
                  | CustardTuple [CustardValue]
                  | CustardMaybe (Maybe CustardValue)
                  | CustardOrdering Ordering
                  | CustardMap (Map (String, String) CustardValue)
                  | CustardData ByteString
                  | CustardLambda (Maybe (String, String))
                                  [CustardParameter]
                                  (Map (String, String) CustardValue)
                                  CustardExpression
                  | CustardNativeLambda (String, String)
                                        (CustardContext
                                         -> [CustardValue]
                                         -> FruitTart (CustardContext,
                                                       CustardValue))
                  | CustardHostAddress HostAddress
                  | CustardHTTPHeader Header
                  | CustardHTTPCookie Cookie

instance Show CustardValue where
  show CustardNull = "Null"
  show (CustardSymbol moduleName properName)
    = "quote(" ++ moduleName ++ "." ++ properName ++ ")"
  show (CustardBool True) = "True"
  show (CustardBool False) = "False"
  show (CustardInteger value) = show value
  show (CustardCharacter value) = ['\'', value, '\'']
  show (CustardString value) = show value
  show (CustardList items) = "[" ++ intercalate ", " (map show items) ++ "]"
  show (CustardMaybe Nothing) = "Nothing"
  show (CustardMaybe (Just value)) = "Just(" ++ show value ++ ")"
  show (CustardOrdering GT) = "GT"
  show (CustardOrdering LT) = "LT"
  show (CustardOrdering EQ) = "EQ"
  show (CustardMap _) = "<Map>"
  show (CustardLambda Nothing _ _ _)
    = "<Lambda>"
  show (CustardLambda (Just (moduleName, properName)) _ _ _)
    = "<Lambda " ++ moduleName ++ "." ++ properName ++ ">"
  show (CustardNativeLambda (moduleName, properName) _)
    = "<NativeLambda " ++ moduleName ++ "." ++ properName ++ ">"

data CustardExpression = CustardLiteral CustardValue
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
                        | CustardQuoteExpression [CustardExpression]
                        | CustardIfExpression CustardExpression
                                              CustardExpression
                                              CustardExpression
                        | CustardCaseExpression [CustardExpression]
                        | CustardCallExpression [CustardExpression]
                        | CustardCallBySymbolExpression [CustardExpression]
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
                        | CustardLetExpression [CustardExpression]
                        | CustardLetMapExpression [CustardExpression]
                        | CustardLetQuery1Expression [CustardExpression]
                        | CustardLetQueryNExpression [CustardExpression]
                        | CustardBlock [CustardExpression]

data CustardContext = CustardContext {
    custardContextType :: CustardContextType,
    custardContextFormInputMap :: Map String String,
    custardContextParameters :: [CustardValue],
    custardContextLexicalBindings :: Map (String, String) CustardValue,
    custardContextGlobalBindings :: Map (String, String) CustardValue
  }

data CustardContextType = ControllerContext | TemplateContext

data CustardToken = TokenValue CustardValue
                   | TokenSymbol String String
                   | TokenQuote
                   | TokenIf
                   | TokenElse
                   | TokenCase
                   | TokenCall
                   | TokenCallBySymbol
                   | TokenIterate
                   | TokenQuery
                   | TokenBound
                   | TokenBind
                   | TokenBindMap
                   | TokenBindQuery1
                   | TokenBindQueryN
                   | TokenLet
                   | TokenLetMap
                   | TokenLetQuery1
                   | TokenLetQueryN
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
