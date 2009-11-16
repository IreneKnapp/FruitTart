{-# LANGUAGE DeriveDataTypeable, ExistentialQuantification, TypeSynonymInstances,
             FlexibleInstances, OverlappingInstances #-}
module Network.FruitTart.Base.Templates.Types (
                                               TemplateValueType(..),
                                               TemplateValue(..),
                                               TemplateParameter(..),
                                               TemplateExpression(..),
                                               TemplateToken(..),
                                               Bindable(..),
                                               AnyBindable(..)
                                              )
    where

import Data.Int
import Data.Map (Map)
import Data.Typeable

import Network.FruitTart.Util


data TemplateValueType = TBool
                       | TInt
                       | TString
                       | TMaybeInt
                       | TMaybeString
                         deriving (Typeable);

data TemplateParameter = TemplateParameter (String, String)

data TemplateValue = TemplateBool Bool
                   | TemplateInteger Int64
                   | TemplateString String
                   | TemplateList [TemplateValue]
                   | TemplateMaybe (Maybe TemplateValue)
                   | TemplateOrdering Ordering
                   | TemplateMap (Map (String, String) TemplateValue)
                   | TemplateLambda [TemplateParameter] TemplateExpression
                   | TemplateNativeLambda (Map (String, String) TemplateValue
                                           -> [TemplateValue]
                                           -> FruitTart TemplateValue)
                     deriving (Typeable)
instance Eq TemplateValue where
    (==) (TemplateBool a) (TemplateBool b) = (==) a b
    (==) (TemplateInteger a) (TemplateInteger b) = (==) a b
    (==) (TemplateString a) (TemplateString b) = (==) a b
    (==) (TemplateList a) (TemplateList b) = (==) a b
    (==) (TemplateMaybe a) (TemplateMaybe b) = (==) a b
    (==) (TemplateOrdering a) (TemplateOrdering b) = (==) a b
    (==) _ _ = False

data TemplateExpression = TemplateLiteral TemplateValue
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
                        | TemplateBoundExpression [TemplateExpression]
                        | TemplateFunctionCall TemplateExpression
                                               [TemplateExpression]
                        | TemplateVariable (String, String)
                        | TemplateLambdaExpression [TemplateParameter] TemplateExpression
                        | TemplateSequence TemplateExpression TemplateExpression

data TemplateToken = TokenValue TemplateValue
                   | TokenSymbol String String
                   | TokenIf
                   | TokenCase
                   | TokenCall
                   | TokenIterate
                   | TokenBound
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

class Bindable a where
    toTemplate :: a -> TemplateValue
data AnyBindable = forall a . Bindable a => AnyBindable a deriving (Typeable)
instance Bindable TemplateValue where
    toTemplate value = value
instance Bindable Bool where
    toTemplate bool = TemplateBool bool
instance Bindable Int64 where
    toTemplate int = TemplateInteger int
instance (Bindable a) => Bindable (Maybe a) where
    toTemplate Nothing = TemplateMaybe Nothing
    toTemplate (Just value) = TemplateMaybe $ Just $ toTemplate value
instance (Bindable a) => Bindable [a] where
    toTemplate values = TemplateList $ map toTemplate values
