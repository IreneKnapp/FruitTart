{
module Network.FruitTart.Custard.Syntax (
                                         readExpression,
                                         parser,
                                         lexer,
                                         intern
                                        )
    where

import Control.Monad
import qualified Data.ByteString.UTF8 as UTF8
import qualified Data.ByteString as BS
import Data.Char
import Data.Foldable hiding (elem)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as Set
import Numeric
import Prelude hiding (or)

import {-# SOURCE #-} Network.FruitTart.Custard.Semantics
import Network.FruitTart.Types

}

%name parser TopLevel
%tokentype { CustardToken }
%monad { IO }
%error { parseError }

%token
        character           { TokenValue $$ }
        string              { TokenValue $$ }
        integer             { TokenValue $$ }
        symbol              { TokenSymbol _ _ }
        quote               { TokenQuote }
        if                  { TokenIf }
        else                { TokenElse }
        case                { TokenCase }
        call                { TokenCall }
        callBySymbol        { TokenCallBySymbol }
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


PrimaryExpression     : character
		      { CustardLiteral $1 }
		      | string
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
		      | StatementLikeExpression
		      { $1 }

FunctionCallExpression
                      : FunctionCallExpression '(' ExpressionList ')'
                      { CustardFunctionCall $1 $3 }
                      | quote '(' ExpressionList ')'
                      { CustardQuoteExpression $3 }
                      | case '(' ExpressionList ')'
                      { CustardCaseExpression $3 }
                      | call '(' ExpressionList ')'
                      { CustardCallExpression $3 }
                      | callBySymbol '(' ExpressionList ')'
                      { CustardCallBySymbolExpression $3 }
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

Expression	      : StatementLikeExpression
		      { $1 }
		      | LogicalExpression
		      { $1 }

ExpressionPlusSemicolon
		      : StatementLikeExpression
		      { $1 }
		      | LogicalExpression ';'
		      { $1 }

StatementLikeExpression
                      : if '(' Expression ')' ExpressionPlusSemicolon
		        else ExpressionPlusSemicolon
                      { CustardIfExpression $3 $5 $7 }
		      | StatementBlock
		      { CustardBlock $1 }

StatementBlock        : '{' StatementList '}'
		      { $2 }

StatementList         : StatementList ExpressionPlusSemicolon
                      { $1 ++ [$2] }
                      | ExpressionPlusSemicolon
		      { [$1] }

TopLevel              : StatementList
		      { CustardBlock $1 }

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

readExpression :: Design -> String -> String -> IO CustardExpression
readExpression design defaultPackage input = do
  tokens <- lexer design defaultPackage input
  parser tokens


parseError :: [CustardToken] -> IO a
parseError _ = error $ "Expression-parsing error."


lexer :: Design -> String -> String -> IO [CustardToken]
lexer _ _ "" = return []
lexer design defaultPackage ('(':rest) = do
  restTokens <- lexer design defaultPackage rest
  return $ TokenLeftParen : restTokens
lexer design defaultPackage (')':rest) = do
  restTokens <- lexer design defaultPackage rest
  return $ TokenRightParen : restTokens
lexer design defaultPackage ('[':rest) = do
  restTokens <- lexer design defaultPackage rest
  return $ TokenLeftSquareBracket : restTokens
lexer design defaultPackage (']':rest) = do
  restTokens <- lexer design defaultPackage rest
  return $ TokenRightSquareBracket : restTokens
lexer design defaultPackage ('{':rest) = do
  restTokens <- lexer design defaultPackage rest
  return $ TokenLeftCurlyBracket : restTokens
lexer design defaultPackage ('}':rest) = do
  restTokens <- lexer design defaultPackage rest
  return $ TokenRightCurlyBracket : restTokens
lexer design defaultPackage ('-':('>':rest)) = do
  restTokens <- lexer design defaultPackage rest
  return $ TokenMinusGreater : restTokens
lexer design defaultPackage (';':rest) = do
  restTokens <- lexer design defaultPackage rest
  return $ TokenSemicolon : restTokens
lexer design defaultPackage (',':rest) = do
  restTokens <- lexer design defaultPackage rest
  return $ TokenComma : restTokens
lexer design defaultPackage ('+':('+':rest)) = do
  restTokens <- lexer design defaultPackage rest
  return $ TokenPlusPlus : restTokens
lexer design defaultPackage ('=':('=':rest)) = do
  restTokens <- lexer design defaultPackage rest
  return $ TokenEqualsEquals : restTokens
lexer design defaultPackage ('!':('=':rest)) = do
  restTokens <- lexer design defaultPackage rest
  return $ TokenExclamationEquals : restTokens
lexer design defaultPackage ('!':rest) = do
  restTokens <- lexer design defaultPackage rest
  return $ TokenExclamation : restTokens
lexer design defaultPackage ('&':('&':rest)) = do
  restTokens <- lexer design defaultPackage rest
  return $ TokenAmpersandAmpersand : restTokens
lexer design defaultPackage ('|':('|':rest)) = do
  restTokens <- lexer design defaultPackage rest
  return $ TokenBarBar : restTokens
lexer design defaultPackage ('>':('=':rest)) = do
  restTokens <- lexer design defaultPackage rest
  return $ TokenGreaterEquals : restTokens
lexer design defaultPackage ('>':rest) = do
  restTokens <- lexer design defaultPackage rest
  return $ TokenGreater : restTokens
lexer design defaultPackage ('<':('=':rest)) = do
  restTokens <- lexer design defaultPackage rest
  return $ TokenLessEquals : restTokens
lexer design defaultPackage ('<':rest) = do
  restTokens <- lexer design defaultPackage rest
  return $ TokenLess : restTokens
lexer design defaultPackage ('+':rest) = do
  restTokens <- lexer design defaultPackage rest
  return $ TokenPlus : restTokens
lexer design defaultPackage ('-':rest) = do
  restTokens <- lexer design defaultPackage rest
  return $ TokenMinus : restTokens
lexer design defaultPackage ('*':rest) = do
  restTokens <- lexer design defaultPackage rest
  return $ TokenStar : restTokens
lexer design defaultPackage ('/':rest) = do
  restTokens <- lexer design defaultPackage rest
  return $ TokenSlash : restTokens
lexer design defaultPackage all@('\'':_) = do
  let (character, rest) = readCharacter all
  restTokens <- lexer design defaultPackage rest
  return $ (TokenValue $ CustardCharacter character) : restTokens
lexer design defaultPackage all@('"':_) = do
  let (string, rest) = readString all
  restTokens <- lexer design defaultPackage rest
  return $ (TokenValue $ CustardString $ UTF8.fromString string) : restTokens
lexer design defaultPackage all@(c:_)
  | isDigit c = do
    let [(result, rest)] = readDec all
    restTokens <- lexer design defaultPackage rest
    return $ (TokenValue $ CustardInteger result) : restTokens
  | isAlpha c = do
    let (maybePackage, symbol, rest) = readSymbol all
    token <- case maybePackage of
      Nothing -> case symbol of
                   _ | symbol == "quote" -> return TokenQuote
                     | symbol == "if" -> return TokenIf
                     | symbol == "else" -> return TokenElse
                     | symbol == "case" -> return TokenCase
                     | symbol == "call" -> return TokenCall
                     | symbol == "callBySymbol" -> return TokenCallBySymbol
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
                     | otherwise -> intern design defaultPackage symbol
      Just package -> intern design package symbol
    restTokens <- lexer design defaultPackage rest
    return $ token : restTokens
  | isSpace c = lexer design defaultPackage $ drop 1 all
  | otherwise = error $ "Expression-lexing error: Unexpected character '"
                        ++ [c] ++ "'."


readCharacter :: String -> (Char, String)
readCharacter input
    = readCharacter' $ drop 1 input
      where readCharacter' ('\\':('\\':('\'':rest))) = ('\\', rest)
            readCharacter' ('\\':('\'':('\'':rest))) = ('\'', rest)
            readCharacter' ('\\':('n':('\'':rest))) = ('\n', rest)
            readCharacter' ('\\':('r':('\'':rest))) = ('\r', rest)
            readCharacter' ('\\':('t':('\'':rest))) = ('\t', rest)
            readCharacter' ('\\':('f':('\'':rest))) = ('\f', rest)
            readCharacter' ('\\':('v':('\'':rest))) = ('\v', rest)
            readCharacter' ('\\':(c:_))
              = error $ "Expression-lexing error: Unknown backslash escape \\"
                        ++ [c] ++ "."
            readCharacter' ('\'':rest)
              = error $ "Expression-lexing error: Empty character literal."
            readCharacter' (c:('\'':rest)) = (c, rest)
            readCharacter' (c:_)
              = error $ "Expression-lexing error: Character literal too long."
            readCharacter' ""
              = error $ "Expression-lexing error: Unterminated character literal."


readString :: String -> (String, String)
readString input
    = readString' $ drop 1 input
      where readString' ('\\':('\\':rest)) = let (a, b) = readString' rest
                                             in ("\\" ++ a, b)
            readString' ('\\':('"':rest)) = let (a, b) = readString' rest
                                            in ("\"" ++ a, b)
            readString' ('\\':('n':rest)) = let (a, b) = readString' rest
                                            in ("\n" ++ a, b)
            readString' ('\\':('r':rest)) = let (a, b) = readString' rest
                                            in ("\r" ++ a, b)
            readString' ('\\':('t':rest)) = let (a, b) = readString' rest
                                            in ("\t" ++ a, b)
            readString' ('\\':('f':rest)) = let (a, b) = readString' rest
                                            in ("\f" ++ a, b)
            readString' ('\\':('v':rest)) = let (a, b) = readString' rest
                                            in ("\v" ++ a, b)
            readString' ('\\':(c:_))
                = error $ "Expression-lexing error: Unknown backslash escape \\"
                        ++ [c] ++ "."
            readString' ('"':rest) = ("", rest)
            readString' (c:rest) = let (a, b) = readString' rest
                                   in ([c] ++ a, b)
            readString' ""
              = error $ "Expression-lexing error: Unterminated string literal."


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


intern :: Design -> String -> String -> IO CustardToken
intern design moduleName properName = do
  let visitModule visitedModules moduleName allowInternal = do
        if elem moduleName visitedModules
          then return (Nothing, visitedModules)
          else do
            visitedModules <- return $ moduleName : visitedModules
            let foundHere = computeSymbolExists design
                                                (moduleName, properName)
            if foundHere && allowInternal
              then return (Just moduleName, visitedModules)
              else do
                let symbolExported =
                      computeSymbolExported design (moduleName, properName)
                if foundHere && symbolExported
                  then return (Just moduleName, visitedModules)
                  else do
                    let importedModules =
                          computeImportedModules design moduleName
                    (maybeResult, visitedModules)
                      <- foldM (\(maybeResult, visitedModules) importedModule
                                -> do
                                  case maybeResult of
                                    Just _ -> return (maybeResult,
                                                      visitedModules)
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


computeImportedModules :: Design -> String -> [String]
computeImportedModules design importingModule =
  let Design { designModules = modules } = design
  in case Map.lookup importingModule modules of
       Nothing -> error $ "Module " ++ importingModule ++ " not found."
       Just (Module { moduleImports = imports }) -> Set.toList imports


computeSymbolExists :: Design -> (String, String) -> Bool
computeSymbolExists design (moduleName, properName) =
  let Design { designModules = modules } = design
  in case Map.lookup moduleName modules of
       Nothing -> error $ "Module " ++ moduleName ++ " not found."
       Just (Module { moduleDefined = defined }) ->
         Set.member properName defined


computeSymbolExported :: Design -> (String, String) -> Bool
computeSymbolExported design (moduleName, properName) =
  let Design { designModules = modules } = design
  in case Map.lookup moduleName modules of
       Nothing -> error $ "Module " ++ moduleName ++ " not found."
       Just (Module { moduleExports = exports }) ->
         Set.member properName exports

}
