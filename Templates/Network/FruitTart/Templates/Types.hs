{-# LANGUAGE DeriveDataTypeable, ExistentialQuantification, TypeSynonymInstances,
             FlexibleInstances, OverlappingInstances #-}
module Network.FruitTart.Templates.Types (
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
                     deriving (Eq, Show)

class Bindable a where
    toTemplate :: a -> TemplateValue
data AnyBindable = forall a . Bindable a => AnyBindable a deriving (Typeable)
instance Bindable Bool where
    toTemplate bool = TemplateBool bool
instance Bindable Int64 where
    toTemplate int = TemplateInteger int
instance Bindable String where
    toTemplate string = TemplateString string
instance (Bindable a) => Bindable (Maybe a) where
    toTemplate Nothing = TemplateMaybe Nothing
    toTemplate (Just value) = TemplateMaybe $ Just $ toTemplate value
instance (Bindable a) => Bindable [a] where
    toTemplate values = TemplateList $ map toTemplate values
instance Bindable [Map (String, String) TemplateValue] where
    toTemplate rows = TemplateList $ map TemplateMap rows
