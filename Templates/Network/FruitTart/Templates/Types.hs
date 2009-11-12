{-# LANGUAGE DeriveDataTypeable #-}
module Network.FruitTart.Templates.Types (
                                          TemplateItemType(..),
                                          TemplateValueType(..),
                                          TemplateValue(..),
                                          TemplateExpression(..),
                                          TemplateToken(..)
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
