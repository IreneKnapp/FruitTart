{
module Network.FruitTart.Base.Templates.Syntax (
                                                readExpression,
                                                parser,
                                                lexer
                                                )
    where

import Data.Char
import Data.Maybe
import Numeric

import Network.FruitTart.Base.Templates.Types

}

%name parser Expression
%tokentype { TemplateToken }
%error { parseError }

%token
        value       { TokenValue $$ }
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

PrimaryExpression     : value
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

readExpression :: String -> String -> TemplateExpression
readExpression defaultPackage input = parser $ lexer defaultPackage input


parseError :: [TemplateToken] -> a
parseError _ = error $ "Expression-parsing error."


lexer :: String -> String -> [TemplateToken]
lexer _ "" = []
lexer defaultPackage ('(':rest) = TokenLeftParen : lexer defaultPackage rest
lexer defaultPackage (')':rest) = TokenRightParen : lexer defaultPackage rest
lexer defaultPackage ('[':rest) = TokenLeftSquareBracket : lexer defaultPackage rest
lexer defaultPackage (']':rest) = TokenRightSquareBracket : lexer defaultPackage rest
lexer defaultPackage ('{':rest) = TokenLeftCurlyBracket : lexer defaultPackage rest
lexer defaultPackage ('}':rest) = TokenRightCurlyBracket : lexer defaultPackage rest
lexer defaultPackage ('-':('>':rest)) = TokenMinusGreater : lexer defaultPackage rest
lexer defaultPackage (';':rest) = TokenSemicolon : lexer defaultPackage rest
lexer defaultPackage (',':rest) = TokenComma : lexer defaultPackage rest
lexer defaultPackage ('+':('+':rest)) = TokenPlusPlus : lexer defaultPackage rest
lexer defaultPackage ('=':('=':rest)) = TokenEqualsEquals : lexer defaultPackage rest
lexer defaultPackage ('!':('=':rest)) = TokenExclamationEquals : lexer defaultPackage rest
lexer defaultPackage ('!':rest) = TokenExclamation : lexer defaultPackage rest
lexer defaultPackage ('&':('&':rest)) = TokenAmpersandAmpersand
                                        : lexer defaultPackage rest
lexer defaultPackage ('|':('|':rest)) = TokenBarBar : lexer defaultPackage rest
lexer defaultPackage ('>':('=':rest)) = TokenGreaterEquals : lexer defaultPackage rest
lexer defaultPackage ('>':rest) = TokenGreater : lexer defaultPackage rest
lexer defaultPackage ('<':('=':rest)) = TokenLessEquals : lexer defaultPackage rest
lexer defaultPackage ('<':rest) = TokenLess : lexer defaultPackage rest
lexer defaultPackage ('+':rest) = TokenPlus : lexer defaultPackage rest
lexer defaultPackage ('-':rest) = TokenMinus : lexer defaultPackage rest
lexer defaultPackage ('*':rest) = TokenStar : lexer defaultPackage rest
lexer defaultPackage ('/':rest) = TokenSlash : lexer defaultPackage rest
lexer defaultPackage all@('"':_) = let (string, rest) = readString all
                                   in (TokenValue $ TemplateString string)
                                      : lexer defaultPackage rest
lexer defaultPackage all@(c:_)
  | isDigit c = let [(result, rest)] = readDec all
                in (TokenValue $ TemplateInteger result) : lexer defaultPackage rest
  | isAlpha c = let (maybePackage, symbol, rest) = readSymbol all
                in case maybePackage of
                     Nothing -> case symbol of
                       _ | symbol == "if" -> TokenIf
                                             : lexer defaultPackage rest
                         | symbol == "case" -> TokenCase
                                               : lexer defaultPackage rest
                         | symbol == "call" -> TokenCall
                                               : lexer defaultPackage rest
                         | symbol == "iterate" -> TokenIterate
                                                  : lexer defaultPackage rest
                         | symbol == "query" -> TokenQuery
                                                : lexer defaultPackage rest
                         | symbol == "lookup" -> TokenLookup
                                                 : lexer defaultPackage rest
                         | symbol == "bound" -> TokenBound
                                                : lexer defaultPackage rest
                         | symbol == "bind" -> TokenBind
                                               : lexer defaultPackage rest
                         | symbol == "bindmap" -> TokenBindMap
                                               : lexer defaultPackage rest
                         | otherwise -> (TokenSymbol defaultPackage symbol)
                                        : lexer defaultPackage rest
                     Just package -> (TokenSymbol package symbol)
                                     : lexer defaultPackage rest
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

}
