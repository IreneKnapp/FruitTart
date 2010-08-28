{
module Network.FruitTart.Custard.Syntax (
                                         readExpression,
                                         parser,
                                         lexer,
                                         intern
                                        )
    where

import Control.Monad
import Data.Char
import Data.Foldable hiding (elem)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import Numeric

import Database.SQLite3
import {-# SOURCE #-} Network.FruitTart.Custard.Semantics
import Network.FruitTart.Types
import Network.FruitTart.Util

}

%name parser Expression
%tokentype { CustardToken }
%monad { IO }
%error { parseError }

%token
        string              { TokenValue $$ }
        integer             { TokenValue $$ }
        symbol              { TokenSymbol _ _ }
        quote               { TokenQuote }
        if                  { TokenIf }
        case                { TokenCase }
        call                { TokenCall }
        iterate             { TokenIterate }
        query               { TokenQuery }
        bound               { TokenBound }
        bind                { TokenBind }
        bindMap             { TokenBindMap }
        bindQuery1          { TokenBindQuery1 }
        bindQueryN          { TokenBindQueryN }
        let                 { TokenLet }
        letMap              { TokenLetMap }
        letQuery1           { TokenLetQuery1 }
        letQueryN           { TokenLetQueryN }
        '('                 { TokenLeftParen }
        ')'                 { TokenRightParen }
        '['                 { TokenLeftSquareBracket }
        ']'                 { TokenRightSquareBracket }
        '{'                 { TokenLeftCurlyBracket }
        '}'                 { TokenRightCurlyBracket }
	'->'                { TokenMinusGreater }
        ';'                 { TokenSemicolon }
        ','                 { TokenComma }
        '++'                { TokenPlusPlus }
        '=='                { TokenEqualsEquals }
        '!='                { TokenExclamationEquals }
        '!'                 { TokenExclamation }
        '&&'                { TokenAmpersandAmpersand }
        '||'                { TokenBarBar }
        '>='                { TokenGreaterEquals }
        '>'                 { TokenGreater }
        '<='                { TokenLessEquals }
        '<'                 { TokenLess }
        '+'                 { TokenPlus }
        '-'                 { TokenMinus }
        '*'                 { TokenStar }
        '/'                 { TokenSlash }

%%

PrimaryExpression     : string
                      { CustardLiteral $1 }
                      | integer
                      { CustardLiteral $1 }
                      | symbol
                      { CustardVariable (symbolTokenPackage $1, symbolTokenName $1) }
                      | '[' ExpressionList ']'
                      { CustardExpressionList $2 }
		      | '{' LambdaList '->' Expression '}'
		      { CustardLambdaExpression $2 $4 }
                      | '(' Expression ')'
                      { $2 }

FunctionCallExpression
                      : FunctionCallExpression '(' ExpressionList ')'
                      { CustardFunctionCall $1 $3 }
                      | quote '(' ExpressionList ')'
                      { CustardQuoteExpression $3 }
                      | if '(' ExpressionList ')'
                      { CustardIfExpression $3 }
                      | case '(' ExpressionList ')'
                      { CustardCaseExpression $3 }
                      | call '(' ExpressionList ')'
                      { CustardCallExpression $3 }
                      | iterate '(' ExpressionList ')'
                      { CustardIterateExpression $3 }
                      | query '(' ExpressionList ')'
                      { CustardQueryExpression $3 }
                      | bound '(' ExpressionList ')'
                      { CustardBoundExpression $3 }
                      | bind '(' ExpressionList ')'
                      { CustardBindExpression $3 }
                      | bindMap '(' ExpressionList ')'
                      { CustardBindMapExpression $3 }
                      | bindQuery1 '(' ExpressionList ')'
                      { CustardBindQuery1Expression $3 }
                      | bindQueryN '(' ExpressionList ')'
                      { CustardBindQueryNExpression $3 }
                      | let '(' ExpressionList ')'
                      { CustardLetExpression $3 }
                      | letMap '(' ExpressionList ')'
                      { CustardLetMapExpression $3 }
                      | letQuery1 '(' ExpressionList ')'
                      { CustardLetQuery1Expression $3 }
                      | letQueryN '(' ExpressionList ')'
                      { CustardLetQueryNExpression $3 }
		      | PrimaryExpression
		      { $1 }

UnaryExpression       : '!' UnaryExpression
                      { CustardOperationNot $2 }
		      | FunctionCallExpression
		      { $1 }

MultiplicativeExpression
                      : MultiplicativeExpression '*' UnaryExpression
                      { CustardOperationMultiply $1 $3 }
                      | MultiplicativeExpression '/' UnaryExpression
                      { CustardOperationDivide $1 $3 }
                      | UnaryExpression
                      { $1 }

AdditiveExpression    : AdditiveExpression '++' MultiplicativeExpression
                      { CustardOperationConcatenate $1 $3 }
                      | AdditiveExpression '+' MultiplicativeExpression
                      { CustardOperationAdd $1 $3 }
                      | AdditiveExpression '-' MultiplicativeExpression
                      { CustardOperationSubtract $1 $3 }
		      | MultiplicativeExpression
		      { $1 }

EqualityExpression    : EqualityExpression '==' AdditiveExpression
                      { CustardOperationEquals $1 $3 }
                      | EqualityExpression '!=' AdditiveExpression
                      { CustardOperationNotEquals $1 $3 }
                      | EqualityExpression '>=' AdditiveExpression
                      { CustardOperationGreaterEquals $1 $3 }
                      | EqualityExpression '>' AdditiveExpression
                      { CustardOperationGreater $1 $3 }
                      | EqualityExpression '<=' AdditiveExpression
                      { CustardOperationLessEquals $1 $3 }
                      | EqualityExpression '<' AdditiveExpression
                      { CustardOperationLess $1 $3 }
                      | AdditiveExpression
                      { $1 }

LogicalExpression     : LogicalExpression '&&' EqualityExpression
                      { CustardOperationAnd $1 $3 }
                      | LogicalExpression '||' EqualityExpression
                      { CustardOperationOr $1 $3 }
                      | EqualityExpression
                      { $1 }

Expression	      : Expression ';' LogicalExpression
		      { CustardSequence $1 $3 }
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
		        ++ [CustardParameter (symbolTokenPackage $3,
			                       symbolTokenName $3)] }
		      | symbol
		      { [CustardParameter (symbolTokenPackage $1, symbolTokenName $1)] }

{

readExpression :: Database -> String -> String -> IO CustardExpression
readExpression database defaultPackage input = do
  tokens <- lexer database defaultPackage input
  parser tokens


parseError :: [CustardToken] -> IO a
parseError _ = error $ "Expression-parsing error."


lexer :: Database -> String -> String -> IO [CustardToken]
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
  return $ (TokenValue $ CustardString string) : restTokens
lexer database defaultPackage all@(c:_)
  | isDigit c = do
    let [(result, rest)] = readDec all
    restTokens <- lexer database defaultPackage rest
    return $ (TokenValue $ CustardInteger result) : restTokens
  | isAlpha c = do
    let (maybePackage, symbol, rest) = readSymbol all
    token <- case maybePackage of
      Nothing -> case symbol of
                   _ | symbol == "quote" -> return TokenQuote
                     | symbol == "if" -> return TokenIf
                     | symbol == "case" -> return TokenCase
                     | symbol == "call" -> return TokenCall
                     | symbol == "iterate" -> return TokenIterate
                     | symbol == "query" -> return TokenQuery
                     | symbol == "bound" -> return TokenBound
                     | symbol == "bind" -> return TokenBind
                     | symbol == "bindMap" -> return TokenBindMap
                     | symbol == "bindQuery1" -> return TokenBindQuery1
                     | symbol == "bindQueryN" -> return TokenBindQueryN
                     | symbol == "let" -> return TokenLet
                     | symbol == "letMap" -> return TokenLetMap
                     | symbol == "letQuery1" -> return TokenLetQuery1
                     | symbol == "letQueryN" -> return TokenLetQueryN
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
    = let (fullName, rest)
            = span (\c -> (isAlpha c) || (isDigit c) || (c == '.')) input
          (lastComponentReversed, _)
            = span (\c -> (isAlpha c) || (isDigit c)) (reverse fullName)
          lastComponent = reverse lastComponentReversed
          maybeOtherComponents = if (length fullName
                                     > (length lastComponent + 1))
                                   then Just $ take (length fullName
                                                     - length lastComponent
                                                     - 1) fullName
                                   else Nothing
      in (maybeOtherComponents, lastComponent, rest)


symbolTokenPackage :: CustardToken -> String
symbolTokenPackage (TokenSymbol result _) = result


symbolTokenName :: CustardToken -> String
symbolTokenName (TokenSymbol _ result) = result


intern :: Database -> String -> String -> IO CustardToken
intern database moduleName properName = do
  let visitModule visitedModules moduleName allowInternal = do
        if elem moduleName visitedModules
          then return (Nothing, visitedModules)
          else do
            visitedModules <- return $ moduleName : visitedModules
            foundHere <- getSymbolExists database (moduleName, properName)
            if foundHere && allowInternal
              then return (Just moduleName, visitedModules)
              else do
                symbolExported <- getSymbolExported database
                                                    (moduleName, properName)
                if foundHere && symbolExported
                  then return (Just moduleName, visitedModules)
                  else do
                    importedModules <- getImportedModules database moduleName
                    (maybeResult, visitedModules)
                      <- foldM (\(maybeResult, visitedModules) importedModule -> do
                                  case maybeResult of
                                    Just _ -> return (maybeResult, visitedModules)
                                    Nothing -> visitModule visitedModules
                                                           importedModule
                                                           False)
                               (Nothing, visitedModules)
                               importedModules
                    case maybeResult of
                      Just _ -> return (maybeResult, visitedModules)
                      Nothing -> if symbolExported
                                   then return (Just moduleName, visitedModules)
                                   else return (Nothing, visitedModules)
  (maybeDefiningModule, _) <- visitModule [] moduleName True
  case maybeDefiningModule of
    Just definingModule -> return $ TokenSymbol definingModule properName
    Nothing -> return $ TokenSymbol moduleName properName


getImportedModules :: Database -> String -> IO [String]
getImportedModules database importingModule = do
  rows <- earlyQuery database
                     (  "SELECT imported_module FROM module_imports "
                     ++ "WHERE importing_module = ?")
                     [SQLText importingModule]
  return $ map (\[SQLText importedModule] -> importedModule) rows


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