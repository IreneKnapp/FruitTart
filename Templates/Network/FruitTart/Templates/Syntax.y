{
module Network.FruitTart.Templates.Syntax (
                                           readExpression,
					   parser,
					   lexer
                                          )
    where

import Data.Char
import Numeric

import Network.FruitTart.Templates.Types

}

%name parser
%tokentype { TemplateToken }
%error { parseError }

%token
        value       { TokenValue $$ }
        symbol	    { TokenSymbol _ _ }
        '('         { TokenLeftParen }
        ')'         { TokenRightParen }
        '['         { TokenLeftSquareBracket }
        ']'         { TokenRightSquareBracket }
        ','         { TokenComma }
        '++'        { TokenPlusPlus }

%%

Expression      : Expression '++' Expression1
		{ TemplateOperationConcatenate $1 $3 }
		| Expression1
		{ $1 }

Expression1	: symbol '(' ExpressionList ')'
		{ TemplateFunctionCall (symbolTokenPackage $1, symbolTokenName $1)
		                       $3 }
		| '[' ExpressionList ']'
		{ TemplateExpressionList $2 }
		| value
		{ TemplateLiteral $1 }
		| symbol
		{ TemplateVariable (symbolTokenPackage $1, symbolTokenName $1) }

ExpressionList	: ExpressionList1
		{ $1 }
		|
		{ [] }

ExpressionList1	: ExpressionList1 ',' Expression
		{ $1 ++ [$3] }
		| Expression
		{ [$1] }

{

readExpression :: String -> String -> TemplateExpression
readExpression defaultPackage input = parser $ lexer defaultPackage input


parseError :: [TemplateToken] -> a
parseError _ = error "Expression-parsing error."


lexer :: String -> String -> [TemplateToken]
lexer _ "" = []
lexer defaultPackage ('(':rest) = TokenLeftParen : lexer defaultPackage rest
lexer defaultPackage (')':rest) = TokenRightParen : lexer defaultPackage rest
lexer defaultPackage ('[':rest) = TokenLeftSquareBracket : lexer defaultPackage rest
lexer defaultPackage (']':rest) = TokenRightSquareBracket : lexer defaultPackage rest
lexer defaultPackage (',':rest) = TokenComma : lexer defaultPackage rest
lexer defaultPackage ('+':('+':rest)) = TokenPlusPlus : lexer defaultPackage rest
lexer defaultPackage all@('"':_) = let (string, rest) = readString all
      		     		   in (TokenValue $ TemplateString string)
				      : lexer defaultPackage rest
lexer defaultPackage all@(c:_)
  | isDigit c = let [(result, rest)] = readDec all
    	      	in (TokenValue $ TemplateInteger result) : lexer defaultPackage rest
  | isUpper c = let (package, symbol, rest) = readSymbolWithPackage all
    	        in (TokenSymbol package symbol) : lexer defaultPackage rest
  | isLower c = let (symbol, rest) = readSymbolComponent all
    	        in (TokenSymbol defaultPackage symbol) : lexer defaultPackage rest
  | isSpace c = lexer defaultPackage $ drop 1 all
  | otherwise = error $ "Expression-lexing error: Unexpected character '" ++ [c] ++ "'."


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


readSymbolComponent :: String -> (String, String)
readSymbolComponent input = span isAlpha input


readSymbolWithPackage :: String -> (String, String, String)
readSymbolWithPackage input
    = readSymbolWithPackage' input
      where readSymbolWithPackage' input
              = let (a, b, c) = readSymbolWithPackage'' input
                in (take (length a - 1) a, b, c)
            readSymbolWithPackage'' all@(c:_)
              | isUpper c
                = let (a, b:rest) = readSymbolComponent all
                  in if b /= '.'
                     then error $ "Expression-lexing error: "
                                ++ "Symbol name missing proper name part."
                     else let (c, d, e) = readSymbolWithPackage'' rest
                          in (a ++ "." ++ c, d, e)
              | isLower c
                = let (a, rest) = readSymbolComponent all
                  in ("", a, rest)
              | otherwise
                = error $ "Expression-lexing error: "
                        ++ "Symbol name has unexpected character '" ++ [c] ++ "'."


symbolTokenPackage :: TemplateToken -> String
symbolTokenPackage (TokenSymbol result _) = result


symbolTokenName :: TemplateToken -> String
symbolTokenName (TokenSymbol _ result) = result

}
