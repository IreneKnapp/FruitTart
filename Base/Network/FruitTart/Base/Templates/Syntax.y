{
module Network.FruitTart.Base.Templates.Syntax (
                                                readExpression,
                                                parser,
                                                lexer
                                                )
    where

import Data.Char
import Data.Foldable
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import Numeric

import Database.SQLite3
import {-# SOURCE #-} Network.FruitTart.Base.Templates.Semantics
import Network.FruitTart.Base.Templates.Types
import Network.FruitTart.Util

}

%name parser Expression
%tokentype { TemplateToken }
%error { parseError }

%token
        string      { TokenStringValue $$ }
        integer     { TokenStringValue $$ }
        symbol      { TokenSymbol _ _ }
        if          { TokenIf }
        case        { TokenCase }
        call        { TokenCall }
        iterate     { TokenIterate }
        query       { TokenQuery }
        lookup      { TokenLookup }
        bound       { TokenBound }
        bind        { TokenBind }
        bindmap     { TokenBindMap }
        '('         { TokenLeftParen }
        ')'         { TokenRightParen }
        '['         { TokenLeftSquareBracket }
        ']'         { TokenRightSquareBracket }
        '{'         { TokenLeftCurlyBracket }
        '}'         { TokenRightCurlyBracket }
	'->'        { TokenMinusGreater }
        ';'         { TokenSemicolon }
        ','         { TokenComma }
        '++'        { TokenPlusPlus }
        '=='        { TokenEqualsEquals }
        '!='        { TokenExclamationEquals }
        '!'         { TokenExclamation }
        '&&'        { TokenAmpersandAmpersand }
        '||'        { TokenBarBar }
        '>='        { TokenGreaterEquals }
        '>'         { TokenGreater }
        '<='        { TokenLessEquals }
        '<'         { TokenLess }
        '+'         { TokenPlus }
        '-'         { TokenMinus }
        '*'         { TokenStar }
        '/'         { TokenSlash }

%%

PrimaryExpression     : string
                      { TemplateLiteral $1 }
                      | integer
                      { TemplateLiteral $1 }
                      | symbol
                      { TemplateVariable (symbolTokenPackage $1, symbolTokenName $1) }
                      | '[' ExpressionList ']'
                      { TemplateExpressionList $2 }
		      | '{' LambdaList '->' Expression '}'
		      { TemplateLambdaExpression $2 $4 }
                      | '(' Expression ')'
                      { $2 }

FunctionCallExpression
                      : FunctionCallExpression '(' ExpressionList ')'
                      { TemplateFunctionCall $1 $3 }
                      | if '(' ExpressionList ')'
                      { TemplateIfExpression $3 }
                      | case '(' ExpressionList ')'
                      { TemplateCaseExpression $3 }
                      | call '(' ExpressionList ')'
                      { TemplateCallExpression $3 }
                      | iterate '(' ExpressionList ')'
                      { TemplateIterateExpression $3 }
                      | query '(' ExpressionList ')'
                      { TemplateQueryExpression $3 }
                      | lookup '(' ExpressionList ')'
                      { TemplateLookupExpression $3 }
                      | bound '(' ExpressionList ')'
                      { TemplateBoundExpression $3 }
                      | bind '(' ExpressionList ')'
                      { TemplateBindExpression $3 }
                      | bindmap '(' ExpressionList ')'
                      { TemplateBindMapExpression $3 }
		      | PrimaryExpression
		      { $1 }

UnaryExpression       : '!' UnaryExpression
                      { TemplateOperationNot $2 }
		      | FunctionCallExpression
		      { $1 }

MultiplicativeExpression
                      : MultiplicativeExpression '*' UnaryExpression
                      { TemplateOperationMultiply $1 $3 }
                      | MultiplicativeExpression '/' UnaryExpression
                      { TemplateOperationDivide $1 $3 }
                      | UnaryExpression
                      { $1 }

AdditiveExpression    : AdditiveExpression '++' MultiplicativeExpression
                      { TemplateOperationConcatenate $1 $3 }
                      | AdditiveExpression '+' MultiplicativeExpression
                      { TemplateOperationAdd $1 $3 }
                      | AdditiveExpression '-' MultiplicativeExpression
                      { TemplateOperationSubtract $1 $3 }
		      | MultiplicativeExpression
		      { $1 }

EqualityExpression    : EqualityExpression '==' AdditiveExpression
                      { TemplateOperationEquals $1 $3 }
                      | EqualityExpression '!=' AdditiveExpression
                      { TemplateOperationNotEquals $1 $3 }
                      | EqualityExpression '>=' AdditiveExpression
                      { TemplateOperationGreaterEquals $1 $3 }
                      | EqualityExpression '>' AdditiveExpression
                      { TemplateOperationGreater $1 $3 }
                      | EqualityExpression '<=' AdditiveExpression
                      { TemplateOperationLessEquals $1 $3 }
                      | EqualityExpression '<' AdditiveExpression
                      { TemplateOperationLess $1 $3 }
                      | AdditiveExpression
                      { $1 }

LogicalExpression     : LogicalExpression '&&' EqualityExpression
                      { TemplateOperationAnd $1 $3 }
                      | LogicalExpression '||' EqualityExpression
                      { TemplateOperationOr $1 $3 }
                      | EqualityExpression
                      { $1 }

Expression	      : Expression ';' LogicalExpression
		      { TemplateSequence $1 $3 }
		      | Expression ';'
		      { $1 }
		      | LogicalExpression
		      { $1 }

ExpressionList        : ExpressionList1
                      { $1 }
                      |
                      { [] }

ExpressionList1       : ExpressionList1 ',' Expression
                      { $1 ++ [$3] }
                      | Expression
                      { [$1] }

LambdaList	      : LambdaList1
		      { $1 }
		      |
		      { [] }

LambdaList1	      : LambdaList1 ',' symbol
		      { $1
		        ++ [TemplateParameter (symbolTokenPackage $3,
			                       symbolTokenName $3)] }
		      | symbol
		      { [TemplateParameter (symbolTokenPackage $1, symbolTokenName $1)] }

{

readExpression :: Database -> String -> String -> IO TemplateExpression
readExpression database defaultPackage input = do
  tokens <- lexer database defaultPackage input
  return $ parser tokens


parseError :: [TemplateToken] -> a
parseError _ = error $ "Expression-parsing error."


lexer :: Database -> String -> String -> IO [TemplateToken]
lexer _ _ "" = return []
lexer database defaultPackage ('(':rest) = do
  restTokens <- lexer database defaultPackage rest
  return $ TokenLeftParen : restTokens
lexer database defaultPackage (')':rest) = do
  restTokens <- lexer database defaultPackage rest
  return $ TokenRightParen : restTokens
lexer database defaultPackage ('[':rest) = do
  restTokens <- lexer database defaultPackage rest
  return $ TokenLeftSquareBracket : restTokens
lexer database defaultPackage (']':rest) = do
  restTokens <- lexer database defaultPackage rest
  return $ TokenRightSquareBracket : restTokens
lexer database defaultPackage ('{':rest) = do
  restTokens <- lexer database defaultPackage rest
  return $ TokenLeftCurlyBracket : restTokens
lexer database defaultPackage ('}':rest) = do
  restTokens <- lexer database defaultPackage rest
  return $ TokenRightCurlyBracket : restTokens
lexer database defaultPackage ('-':('>':rest)) = do
  restTokens <- lexer database defaultPackage rest
  return $ TokenMinusGreater : restTokens
lexer database defaultPackage (';':rest) = do
  restTokens <- lexer database defaultPackage rest
  return $ TokenSemicolon : restTokens
lexer database defaultPackage (',':rest) = do
  restTokens <- lexer database defaultPackage rest
  return $ TokenComma : restTokens
lexer database defaultPackage ('+':('+':rest)) = do
  restTokens <- lexer database defaultPackage rest
  return $ TokenPlusPlus : restTokens
lexer database defaultPackage ('=':('=':rest)) = do
  restTokens <- lexer database defaultPackage rest
  return $ TokenEqualsEquals : restTokens
lexer database defaultPackage ('!':('=':rest)) = do
  restTokens <- lexer database defaultPackage rest
  return $ TokenExclamationEquals : restTokens
lexer database defaultPackage ('!':rest) = do
  restTokens <- lexer database defaultPackage rest
  return $ TokenExclamation : restTokens
lexer database defaultPackage ('&':('&':rest)) = do
  restTokens <- lexer database defaultPackage rest
  return $ TokenAmpersandAmpersand : restTokens
lexer database defaultPackage ('|':('|':rest)) = do
  restTokens <- lexer database defaultPackage rest
  return $ TokenBarBar : restTokens
lexer database defaultPackage ('>':('=':rest)) = do
  restTokens <- lexer database defaultPackage rest
  return $ TokenGreaterEquals : restTokens
lexer database defaultPackage ('>':rest) = do
  restTokens <- lexer database defaultPackage rest
  return $ TokenGreater : restTokens
lexer database defaultPackage ('<':('=':rest)) = do
  restTokens <- lexer database defaultPackage rest
  return $ TokenLessEquals : restTokens
lexer database defaultPackage ('<':rest) = do
  restTokens <- lexer database defaultPackage rest
  return $ TokenLess : restTokens
lexer database defaultPackage ('+':rest) = do
  restTokens <- lexer database defaultPackage rest
  return $ TokenPlus : restTokens
lexer database defaultPackage ('-':rest) = do
  restTokens <- lexer database defaultPackage rest
  return $ TokenMinus : restTokens
lexer database defaultPackage ('*':rest) = do
  restTokens <- lexer database defaultPackage rest
  return $ TokenStar : restTokens
lexer database defaultPackage ('/':rest) = do
  restTokens <- lexer database defaultPackage rest
  return $ TokenSlash : restTokens
lexer database defaultPackage all@('"':_) = do
  let (string, rest) = readString all
  restTokens <- lexer database defaultPackage rest
  return $ (TokenStringValue $ TemplateString string) : restTokens
lexer database defaultPackage all@(c:_)
  | isDigit c = do
    let [(result, rest)] = readDec all
    restTokens <- lexer database defaultPackage rest
    return $ (TokenIntegerValue $ TemplateInteger result) : restTokens
  | isAlpha c = do
    let (maybePackage, symbol, rest) = readSymbol all
    token <- case maybePackage of
      Nothing -> case symbol of
                   _ | symbol == "if" -> return TokenIf
                     | symbol == "case" -> return TokenCase
                     | symbol == "call" -> return TokenCall
                     | symbol == "iterate" -> return TokenIterate
                     | symbol == "query" -> return TokenQuery
                     | symbol == "lookup" -> return TokenLookup
                     | symbol == "bound" -> return TokenBound
                     | symbol == "bind" -> return TokenBind
                     | symbol == "bindmap" -> return TokenBindMap
                     | otherwise -> intern database defaultPackage symbol
      Just package -> intern database package symbol
    restTokens <- lexer database defaultPackage rest
    return $ token : restTokens
  | isSpace c = lexer database defaultPackage $ drop 1 all
  | otherwise = error $ "Expression-lexing error: Unexpected character '"
                        ++ [c] ++ "'."


readString :: String -> (String, String)
readString input
    = readString' $ drop 1 input
      where readString' ('\\':('\\':rest)) = let (a, b) = readString' rest
                                             in ("\\" ++ a, b)
            readString' ('\\':('"':rest)) = let (a, b) = readString' rest
                                            in ("\"" ++ a, b)
            readString' ('\\':('n':rest)) = let (a, b) = readString' rest
                                            in ("\n" ++ a, b)
            readString' ('\\':(c:_))
                = error $ "Expression-lexing error: Unknown backslash escape \\"
                        ++ [c] ++ "."
            readString' ('"':rest) = ("", rest)
            readString' (c:rest) = let (a, b) = readString' rest
                                   in ([c] ++ a, b)


readSymbol :: String -> (Maybe String, String, String)
readSymbol input
    = let (fullName, rest) = span (\c -> (isAlpha c) || (c == '.')) input
          (lastComponentReversed, _) = span isAlpha (reverse fullName)
          lastComponent = reverse lastComponentReversed
          maybeOtherComponents = if (length fullName > (length lastComponent + 1))
                                   then Just $ take (length fullName
                                                     - length lastComponent
                                                     - 1) fullName
                                   else Nothing
      in (maybeOtherComponents, lastComponent, rest)


symbolTokenPackage :: TemplateToken -> String
symbolTokenPackage (TokenSymbol result _) = result


symbolTokenName :: TemplateToken -> String
symbolTokenName (TokenSymbol _ result) = result


intern :: Database -> String -> String -> IO TemplateToken
intern database moduleName properName = do
  importedModules
    <- earlyQuery database
                  (  "SELECT imported_module FROM module_imports "
                  ++ "WHERE importing_module = ?")
                  [SQLText moduleName]
  maybeDefiningModule
    <- foldlM (\maybeDefiningModule (importedModule, allowInternal) -> do
                 case maybeDefiningModule of
                   Just _ -> return maybeDefiningModule
                   Nothing -> do
                     foundHere <- getSymbolExists database
                                                  (importedModule, properName)
                     visible
                       <- if foundHere
                            then if allowInternal
                              then return True
                              else getSymbolExported database
                                                     (importedModule,
                                                      properName)
                            else return False
                     return $ if visible
                                then Just importedModule
                                else Nothing)
              Nothing
              $ (moduleName, True)
                : map (\[SQLText importedModule] -> (importedModule, False))
                      importedModules
  case maybeDefiningModule of
    Just definingModule -> return $ TokenSymbol definingModule properName
    Nothing -> return $ TokenSymbol moduleName properName


getSymbolExists :: Database -> (String, String) -> IO Bool
getSymbolExists database variableName = do
  let builtinExists' = builtinExists variableName
  if builtinExists'
    then return True
    else do
      functionExists <- getFunctionExists database variableName
      if functionExists
        then return True
        else do
          templateExists <- getTemplateExists database variableName
          if templateExists
            then return True
            else do
              queryExists <- getQueryExists database variableName
              if queryExists
                then return True
                else return False


getSymbolExported :: Database -> (String, String) -> IO Bool
getSymbolExported database (moduleName, properName) = do
  [[SQLInteger count]]
    <- earlyQuery database
                  (  "SELECT count(*) FROM module_exports "
                  ++ "WHERE module = ? AND symbol = ?")
                  [SQLText moduleName, SQLText properName]
  return $ case count of
             0 -> False
             _ -> True


builtinExists :: (String, String) -> Bool
builtinExists variableName =
  case Map.lookup variableName builtinBindings of
    Just _ -> True
    Nothing -> False


getFunctionExists :: Database -> (String, String) -> IO Bool
getFunctionExists database (moduleName, properName) = do
  [[SQLInteger result]] <- earlyQuery database
                                      (  "SELECT count(*) "
                                      ++ "FROM functions "
                                      ++ "WHERE module = ? AND name = ?")
                                      [SQLText moduleName, SQLText properName]
  return $ case result of
             0 -> False
             _ -> True


getTemplateExists :: Database -> (String, String) -> IO Bool
getTemplateExists database (moduleName, properName) = do
  [[SQLInteger result]] <- earlyQuery database
                                      (  "SELECT count(*) "
                                      ++ "FROM templates "
                                      ++ "WHERE module = ? AND name = ?")
                                      [SQLText moduleName, SQLText properName]
  return $ case result of
             0 -> False
             _ -> True


getQueryExists :: Database -> (String, String) -> IO Bool
getQueryExists database (moduleName, properName) = do
  [[SQLInteger result]] <- earlyQuery database
                                      (  "SELECT count(*) "
                                      ++ "FROM queries "
                                      ++ "WHERE module = ? AND name = ?")
                                      [SQLText moduleName, SQLText properName]
  return $ case result of
             0 -> False
             _ -> True

}
