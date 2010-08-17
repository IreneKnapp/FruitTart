{-# LANGUAGE GADTs, EmptyDataDecls #-}
module Network.FruitTart.Base.Templates.Types (
                                               TemplateValueType(..),
                                               TemplateValue(..),
                                               TemplateParameter(..),
                                               TemplateExpression(..),
                                               TemplateContext(..),
                                               TemplateToken(..),
                                              )
    where

import Data.Int
import Data.List
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Typeable

import Network.FruitTart.Util


data TemplateValueType = TBool
                       | TInt
                       | TString
                       | TMaybeInt
                       | TMaybeString

data TemplateParameter = TemplateParameter (String, String)

data TemplateLambdaType

data TemplateValue t where
  TemplateNull :: TemplateValue ()
  TemplateBool :: Bool -> TemplateValue Bool
  TemplateInteger :: Int64 -> TemplateValue Int64
  TemplateCharacter :: Char -> TemplateValue Char
  TemplateString :: String -> TemplateValue String
  TemplateList :: [a] -> TemplateValue [a]
  TemplateMaybe :: (Maybe (TemplateValue a)) -> TemplateValue (Maybe a)
  TemplateOrdering :: Ordering -> TemplateValue Ordering
  TemplateMap :: Map (String, String) (TemplateValue a)
              -> TemplateValue (Map (String, String) a)
  TemplateLambda :: [TemplateParameter]
                 -> Map (String, String) (TemplateValue a)
                 -> TemplateExpression
                 -> TemplateValue TemplateLambdaType
  TemplateNativeLambda :: (TemplateContext
                           -> [TemplateValue a]
                           -> FruitTart (TemplateValue b))
                       -> TemplateValue TemplateLambdaType

data TemplateExpression = TemplateStringLiteral (TemplateValue String)
                        | TemplateIntegerLiteral (TemplateValue Int64)
                        | TemplateExpressionList [TemplateExpression]
                        | TemplateOperationConcatenate TemplateExpression
                                                       TemplateExpression
                        | TemplateOperationEquals TemplateExpression
                                                  TemplateExpression
                        | TemplateOperationNotEquals TemplateExpression
                                                     TemplateExpression
                        | TemplateOperationAnd TemplateExpression TemplateExpression
                        | TemplateOperationOr TemplateExpression TemplateExpression
                        | TemplateOperationNot TemplateExpression
                        | TemplateOperationGreater TemplateExpression
                                                   TemplateExpression
                        | TemplateOperationGreaterEquals TemplateExpression
                                                         TemplateExpression
                        | TemplateOperationLess TemplateExpression
                                                TemplateExpression
                        | TemplateOperationLessEquals TemplateExpression
                                                      TemplateExpression
                        | TemplateOperationAdd TemplateExpression TemplateExpression
                        | TemplateOperationSubtract TemplateExpression TemplateExpression
                        | TemplateOperationMultiply TemplateExpression TemplateExpression
                        | TemplateOperationDivide TemplateExpression TemplateExpression
                        | TemplateIfExpression [TemplateExpression]
                        | TemplateCaseExpression [TemplateExpression]
                        | TemplateCallExpression [TemplateExpression]
                        | TemplateIterateExpression [TemplateExpression]
                        | TemplateQueryExpression [TemplateExpression]
                        | TemplateLookupExpression [TemplateExpression]
                        | TemplateBoundExpression [TemplateExpression]
                        | TemplateFunctionCall TemplateExpression
                                               [TemplateExpression]
                        | TemplateVariable (String, String)
                        | TemplateLambdaExpression [TemplateParameter] TemplateExpression
                        | TemplateBindExpression [TemplateExpression]
                        | TemplateBindMapExpression [TemplateExpression]
                        | TemplateSequence TemplateExpression TemplateExpression

data TemplateContext = TemplateNormalContext
                     | TemplateControllerContext

data TemplateToken = TokenStringValue (TemplateValue String)
                   | TokenIntegerValue (TemplateValue Int64)
                   | TokenSymbol String String
                   | TokenIf
                   | TokenCase
                   | TokenCall
                   | TokenIterate
                   | TokenQuery
                   | TokenLookup
                   | TokenBound
                   | TokenBind
                   | TokenBindMap
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
