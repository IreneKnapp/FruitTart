{-# LANGUAGE DeriveDataTypeable, ExistentialQuantification, TypeSynonymInstances,
             FlexibleInstances, OverlappingInstances #-}
module Network.FruitTart.Base.Templates.Types (
                                               TemplateItemType(..),
                                               TemplateValueType(..),
                                               TemplateValue(..),
                                               TemplateExpression(..),
                                               TemplateToken(..),
                                               Bindable(..),
                                               AnyBindable(..)
                                              )
    where

import Data.Int
import Data.Map (Map)
import Data.Typeable


data TemplateItemType = Content
                      | Expression
                        deriving (Eq, Show);

data TemplateValueType = TBool
                       | TInt
                       | TString
                       | TMaybeInt
                       | TMaybeString
                         deriving (Eq, Show, Typeable);

data TemplateValue = TemplateBool Bool
                   | TemplateInteger Int64
                   | TemplateString String
                   | TemplateList [TemplateValue]
                   | TemplateMaybe (Maybe TemplateValue)
                   | TemplateMap (Map (String, String) TemplateValue)
                     deriving (Eq, Show, Typeable)

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
                        | TemplateFunctionCall (String, String)
                                               [TemplateExpression]
                        | TemplateVariable (String, String)
                          deriving (Eq, Show)

data TemplateToken = TokenValue TemplateValue
                   | TokenSymbol String String
                   | TokenLeftParen
                   | TokenRightParen
                   | TokenLeftSquareBracket
                   | TokenRightSquareBracket
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
                     deriving (Eq, Show)

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
