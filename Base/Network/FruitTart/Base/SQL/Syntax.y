{
{-# LANGUAGE ExistentialQuantification #-}
module Network.FruitTart.Base.SQL.Syntax (
					  readStatementList,
-- 					  readStatement,
					  readExpression,
					  readFromClause,
					  readWhereClause,
					  readGroupClause,
					  readOrderClause,
					  readLimitClause,
					  readWhenClause,
					  readConflictClause,
					  readForeignKeyClause
					 )
    where

import Data.Char
import Data.Maybe
import Numeric

import Network.FruitTart.Base.SQL.Types

import Debug.Trace

}

%name parseStatementList StatementList
%name parseStatement AnyStatement
%name parseExpression Expression
%name parseFromClause FromClause
%name parseWhereClause WhereClause
%name parseGroupClause GroupClause
%name parseOrderClause OrderClause
%name parseLimitClause LimitClause
%name parseWhenClause WhenClause
%name parseConflictClause ConflictClause
%name parseForeignKeyClause ForeignKeyClause
%tokentype { Token }
%error { parseError }

%token
        identifier            { Identifier $$ }
        integer               { LiteralInteger $$ }
        float                 { LiteralFloat $$ }
        string                { LiteralString $$ }
        blob                  { LiteralBlob $$ }
        variable              { Variable }
	variableN             { VariableN $$ }
	variableNamed         { VariableNamed $$ }
	moduleArgumentToken   { ModuleArgumentToken $$ }
	'||'                  { PunctuationBarBar }
	'*'                   { PunctuationStar }
	'/'                   { PunctuationSlash }
	'%'                   { PunctuationPercent }
	'+'                   { PunctuationPlus }
	'-'                   { PunctuationMinus }
	'<<'                  { PunctuationLessLess }
	'>>'                  { PunctuationGreaterGreater }
	'&'                   { PunctuationAmpersand }
	'|'                   { PunctuationBar }
	'<'                   { PunctuationLess }
	'<='                  { PunctuationLessEquals }
	'>'                   { PunctuationGreater }
	'>='                  { PunctuationGreaterEquals }
	'='                   { PunctuationEquals }
	'=='                  { PunctuationEqualsEquals }
	'!='                  { PunctuationBangEquals }
	'<>'                  { PunctuationLessGreater }
	'~'                   { PunctuationTilde }
	'('                   { PunctuationLeftParenthesis }
	')'                   { PunctuationRightParenthesis }
	','                   { PunctuationComma }
	'.'                   { PunctuationDot }
	';'                   { PunctuationSemicolon }
	abort		      { KeywordAbort }
	action                { KeywordAction }
	add           	      { KeywordAdd }
        after   	      { KeywordAfter }
        all   		      { KeywordAll }
        alter   	      { KeywordAlter }
        analyze   	      { KeywordAnalyze }
        and   		      { KeywordAnd }
        as   		      { KeywordAs }
        asc   		      { KeywordAsc }
        attach   	      { KeywordAttach }
        autoincrement         { KeywordAutoincrement }
        before   	      { KeywordBefore }
        begin   	      { KeywordBegin }
        between   	      { KeywordBetween }
        by   		      { KeywordBy }
        cascade   	      { KeywordCascade }
        case   		      { KeywordCase }
        cast   		      { KeywordCast }
        check   	      { KeywordCheck }
        collate   	      { KeywordCollate }
        column   	      { KeywordColumn }
        commit   	      { KeywordCommit }
        conflict   	      { KeywordConflict }
        constraint   	      { KeywordConstraint }
        create   	      { KeywordCreate }
        cross   	      { KeywordCross }
        currentDate   	      { KeywordCurrentDate }
        currentTime   	      { KeywordCurrentTime }
        currentTimestamp      { KeywordCurrentTimestamp }
        database   	      { KeywordDatabase }
        default   	      { KeywordDefault }
        deferrable   	      { KeywordDeferrable }
        deferred   	      { KeywordDeferred }
        delete   	      { KeywordDelete }
        desc   		      { KeywordDesc }
        detach   	      { KeywordDetach }
        distinct   	      { KeywordDistinct }
        drop   		      { KeywordDrop }
        each   		      { KeywordEach }
        else   		      { KeywordElse }
        end   		      { KeywordEnd }
        escape   	      { KeywordEscape }
        except   	      { KeywordExcept }
        exclusive   	      { KeywordExclusive }
        exists   	      { KeywordExists }
        explain   	      { KeywordExplain }
        fail   		      { KeywordFail }
        for   		      { KeywordFor }
        foreign   	      { KeywordForeign }
        from   		      { KeywordFrom }
        full   		      { KeywordFull }
        glob   		      { KeywordGlob }
        group   	      { KeywordGroup }
        having   	      { KeywordHaving }
        if   		      { KeywordIf }
        ignore   	      { KeywordIgnore }
        immediate   	      { KeywordImmediate }
        in   		      { KeywordIn }
        index   	      { KeywordIndex }
        indexed   	      { KeywordIndexed }
        initially   	      { KeywordInitially }
        inner   	      { KeywordInner }
        insert   	      { KeywordInsert }
        instead   	      { KeywordInstead }
        intersect   	      { KeywordIntersect }
        into   		      { KeywordInto }
        is   		      { KeywordIs }
        isnull   	      { KeywordIsnull }
        join   		      { KeywordJoin }
        key   		      { KeywordKey }
        left   		      { KeywordLeft }
        like   		      { KeywordLike }
        limit   	      { KeywordLimit }
        match   	      { KeywordMatch }
        natural   	      { KeywordNatural }
        no   		      { KeywordNo }
        not   		      { KeywordNot }
        notnull   	      { KeywordNotnull }
        null   		      { KeywordNull }
        of   		      { KeywordOf }
        offset   	      { KeywordOffset }
        on   		      { KeywordOn }
        or   		      { KeywordOr }
        order   	      { KeywordOrder }
        outer   	      { KeywordOuter }
        plan   		      { KeywordPlan }
        pragma   	      { KeywordPragma }
        primary   	      { KeywordPrimary }
        query   	      { KeywordQuery }
        raise   	      { KeywordRaise }
        references   	      { KeywordReferences }
        regexp   	      { KeywordRegexp }
        reindex   	      { KeywordReindex }
        release   	      { KeywordRelease }
        rename   	      { KeywordRename }
        replace   	      { KeywordReplace }
        restrict   	      { KeywordRestrict }
        right   	      { KeywordRight }
        rollback   	      { KeywordRollback }
        row   		      { KeywordRow }
        savepoint   	      { KeywordSavepoint }
        select   	      { KeywordSelect }
        set   		      { KeywordSet }
        table   	      { KeywordTable }
        temp   		      { KeywordTemp }
        temporary   	      { KeywordTemporary }
        then   		      { KeywordThen }
        to   		      { KeywordTo }
        transaction   	      { KeywordTransaction }
        trigger   	      { KeywordTrigger }
        union   	      { KeywordUnion }
        unique   	      { KeywordUnique }
        update   	      { KeywordUpdate }
        using   	      { KeywordUsing }
        vacuum   	      { KeywordVacuum }
        values   	      { KeywordValues }
        view   		      { KeywordView }
        virtual   	      { KeywordVirtual }
        when   		      { KeywordWhen }
        where   	      { KeywordWhere }

%%

StatementList         :: { StatementList }
                      : identifier
                      { StatementList [] }

Statement             :: { AnyStatement }
                      : identifier
                      { Statement $ Vacuum }

Expression            :: { Expression }
                      : identifier
                      { ExpressionLiteralNull }

FromClause            :: { FromClause }
                      : identifier
                      { From $ JoinSource
                                 (TableSource (SinglyQualifiedIdentifier Nothing "table")
                                              NoAs
                                              NoIndexedBy)
                                 [] }

WhereClause           :: { WhereClause }
                      : identifier
                      { Where $ ExpressionLiteralNull }

GroupClause           :: { GroupClause }
                      : identifier
                      { GroupBy (fromJust $ mkOneOrMore [OrderingTerm
                                                          ExpressionLiteralNull
                                                          NoCollation
                                                          NoAscDesc])
                                NoHaving }

OrderClause           :: { OrderClause }
                      : identifier
                      { OrderBy (fromJust $ mkOneOrMore [OrderingTerm
                                                          ExpressionLiteralNull
                                                          NoCollation
                                                          NoAscDesc]) }

LimitClause           :: { LimitClause }
                      : identifier
                      { Limit 1 }

WhenClause            :: { WhenClause }
                      : identifier
                      { When $ ExpressionLiteralNull }

ConflictClause        :: { ConflictClause }
                      : identifier
                      { OnConflictRollback }

ForeignKeyClause      :: { ForeignKeyClause }
                      : identifier
                      { References (UnqualifiedIdentifier "foreign")
                                   []
                                   []
                                   Nothing }

{

readStatementList :: String -> StatementList
readStatementList input = parseStatementList $ lexer input


readStatement :: String -> AnyStatement
readStatement input = parseStatement $ lexer input


readExpression :: String -> Expression
readExpression input = parseExpression $ lexer input


readFromClause :: String -> FromClause
readFromClause input = parseFromClause $ lexer input


readWhereClause :: String -> WhereClause
readWhereClause input = parseWhereClause $ lexer input


readGroupClause :: String -> GroupClause
readGroupClause input = parseGroupClause $ lexer input


readOrderClause :: String -> OrderClause
readOrderClause input = parseOrderClause $ lexer input


readLimitClause :: String -> LimitClause
readLimitClause input = parseLimitClause $ lexer input


readWhenClause :: String -> WhenClause
readWhenClause input = parseWhenClause $ lexer input


readConflictClause :: String -> ConflictClause
readConflictClause input = parseConflictClause $ lexer input


readForeignKeyClause :: String -> ForeignKeyClause
readForeignKeyClause input = parseForeignKeyClause $ lexer input


parseError :: [Token] -> a
parseError _ = error $ "SQL-parsing error."


lexer :: String -> [Token]
lexer "" = []
lexer all@('.':c:_) | isDigit c = let (token, rest) = readNumericLiteral all
                                  in token : lexer rest
lexer ('!':'=':rest) = PunctuationBangEquals : lexer rest
lexer ('%':rest) = PunctuationPercent : lexer rest
lexer ('&':rest) = PunctuationAmpersand : lexer rest
lexer ('(':rest) = PunctuationLeftParenthesis : lexer rest
lexer (')':rest) = PunctuationRightParenthesis : lexer rest
lexer ('*':rest) = PunctuationStar : lexer rest
lexer ('+':rest) = PunctuationPlus : lexer rest
lexer (',':rest) = PunctuationComma : lexer rest
lexer ('-':rest) = PunctuationMinus : lexer rest
lexer ('.':rest) = PunctuationDot : lexer rest
lexer ('/':rest) = PunctuationSlash : lexer rest
lexer (';':rest) = PunctuationSemicolon : lexer rest
lexer ('<':'<':rest) = PunctuationLessLess : lexer rest
lexer ('<':'=':rest) = PunctuationLessEquals : lexer rest
lexer ('<':'>':rest) = PunctuationLessGreater : lexer rest
lexer ('<':rest) = PunctuationLess : lexer rest
lexer ('=':'=':rest) = PunctuationEqualsEquals : lexer rest
lexer ('=':rest) = PunctuationEquals : lexer rest
lexer ('>':'=':rest) = PunctuationGreaterEquals : lexer rest
lexer ('>':'>':rest) = PunctuationGreaterGreater : lexer rest
lexer ('>':rest) = PunctuationGreater : lexer rest
lexer ('|':'|':rest) = PunctuationBarBar : lexer rest
lexer ('|':rest) = PunctuationBar : lexer rest
lexer ('~':rest) = PunctuationTilde : lexer rest
lexer all@('\'':_) = let (token, rest) = readStringLiteral all
                     in token : lexer rest
lexer all@(c:_)
  | isDigit c = let (token, rest) = readNumericLiteral all
                in token : lexer rest
  | isAlpha c = let (identifierOrKeyword, rest) = readIdentifierOrKeyword all
                    keyword = map toLower identifierOrKeyword
                    identifier = identifierOrKeyword
                in case identifierOrKeyword of
                  _ | keyword == "abort" -> KeywordAbort : lexer rest
                    | keyword == "action" -> KeywordAction : lexer rest
                    | keyword == "add" -> KeywordAdd : lexer rest
                    | keyword == "after" -> KeywordAfter : lexer rest
                    | keyword == "all" -> KeywordAll : lexer rest
                    | keyword == "alter" -> KeywordAlter : lexer rest
                    | keyword == "analyze" -> KeywordAnalyze : lexer rest
                    | keyword == "and" -> KeywordAnd : lexer rest
                    | keyword == "as" -> KeywordAs : lexer rest
                    | keyword == "asc" -> KeywordAsc : lexer rest
                    | keyword == "attach" -> KeywordAttach : lexer rest
                    | keyword == "autoincrement" -> KeywordAutoincrement : lexer rest
                    | keyword == "before" -> KeywordBefore : lexer rest
                    | keyword == "begin" -> KeywordBegin : lexer rest
                    | keyword == "between" -> KeywordBetween : lexer rest
                    | keyword == "by" -> KeywordBy : lexer rest
                    | keyword == "cascade" -> KeywordCascade : lexer rest
                    | keyword == "case" -> KeywordCase : lexer rest
                    | keyword == "cast" -> KeywordCast : lexer rest
                    | keyword == "check" -> KeywordCheck : lexer rest
                    | keyword == "collate" -> KeywordCollate : lexer rest
                    | keyword == "column" -> KeywordColumn : lexer rest
                    | keyword == "commit" -> KeywordCommit : lexer rest
                    | keyword == "conflict" -> KeywordConflict : lexer rest
                    | keyword == "constraint" -> KeywordConstraint : lexer rest
                    | keyword == "create" -> KeywordCreate : lexer rest
                    | keyword == "cross" -> KeywordCross : lexer rest
                    | keyword == "current_date" -> KeywordCurrentDate : lexer rest
                    | keyword == "current_time" -> KeywordCurrentTime : lexer rest
                    | keyword == "current_timestamp"
                        -> KeywordCurrentTimestamp : lexer rest
                    | keyword == "database" -> KeywordDatabase : lexer rest
                    | keyword == "default" -> KeywordDefault : lexer rest
                    | keyword == "deferrable" -> KeywordDeferrable : lexer rest
                    | keyword == "deferred" -> KeywordDeferred : lexer rest
                    | keyword == "delete" -> KeywordDelete : lexer rest
                    | keyword == "desc" -> KeywordDesc : lexer rest
                    | keyword == "detach" -> KeywordDetach : lexer rest
                    | keyword == "distinct" -> KeywordDistinct : lexer rest
                    | keyword == "drop" -> KeywordDrop : lexer rest
                    | keyword == "each" -> KeywordEach : lexer rest
                    | keyword == "else" -> KeywordElse : lexer rest
                    | keyword == "end" -> KeywordEnd : lexer rest
                    | keyword == "escape" -> KeywordEscape : lexer rest
                    | keyword == "except" -> KeywordExcept : lexer rest
                    | keyword == "exclusive" -> KeywordExclusive : lexer rest
                    | keyword == "exists" -> KeywordExists : lexer rest
                    | keyword == "explain" -> KeywordExplain : lexer rest
                    | keyword == "fail" -> KeywordFail : lexer rest
                    | keyword == "for" -> KeywordFor : lexer rest
                    | keyword == "foreign" -> KeywordForeign : lexer rest
                    | keyword == "from" -> KeywordFrom : lexer rest
                    | keyword == "full" -> KeywordFull : lexer rest
                    | keyword == "glob" -> KeywordGlob : lexer rest
                    | keyword == "group" -> KeywordGroup : lexer rest
                    | keyword == "having" -> KeywordHaving : lexer rest
                    | keyword == "if" -> KeywordIf : lexer rest
                    | keyword == "ignore" -> KeywordIgnore : lexer rest
                    | keyword == "immediate" -> KeywordImmediate : lexer rest
                    | keyword == "in" -> KeywordIn : lexer rest
                    | keyword == "index" -> KeywordIndex : lexer rest
                    | keyword == "indexed" -> KeywordIndexed : lexer rest
                    | keyword == "initially" -> KeywordInitially : lexer rest
                    | keyword == "inner" -> KeywordInner : lexer rest
                    | keyword == "insert" -> KeywordInsert : lexer rest
                    | keyword == "instead" -> KeywordInstead : lexer rest
                    | keyword == "intersect" -> KeywordIntersect : lexer rest
                    | keyword == "into" -> KeywordInto : lexer rest
                    | keyword == "is" -> KeywordIs : lexer rest
                    | keyword == "isnull" -> KeywordIsnull : lexer rest
                    | keyword == "join" -> KeywordJoin : lexer rest
                    | keyword == "key" -> KeywordKey : lexer rest
                    | keyword == "left" -> KeywordLeft : lexer rest
                    | keyword == "like" -> KeywordLike : lexer rest
                    | keyword == "limit" -> KeywordLimit : lexer rest
                    | keyword == "match" -> KeywordMatch : lexer rest
                    | keyword == "natural" -> KeywordNatural : lexer rest
                    | keyword == "no" -> KeywordNo : lexer rest
                    | keyword == "not" -> KeywordNot : lexer rest
                    | keyword == "notnull" -> KeywordNotnull : lexer rest
                    | keyword == "null" -> KeywordNull : lexer rest
                    | keyword == "of" -> KeywordOf : lexer rest
                    | keyword == "offset" -> KeywordOffset : lexer rest
                    | keyword == "on" -> KeywordOn : lexer rest
                    | keyword == "or" -> KeywordOr : lexer rest
                    | keyword == "order" -> KeywordOrder : lexer rest
                    | keyword == "outer" -> KeywordOuter : lexer rest
                    | keyword == "plan" -> KeywordPlan : lexer rest
                    | keyword == "pragma" -> KeywordPragma : lexer rest
                    | keyword == "primary" -> KeywordPrimary : lexer rest
                    | keyword == "query" -> KeywordQuery : lexer rest
                    | keyword == "raise" -> KeywordRaise : lexer rest
                    | keyword == "references" -> KeywordReferences : lexer rest
                    | keyword == "regexp" -> KeywordRegexp : lexer rest
                    | keyword == "reindex" -> KeywordReindex : lexer rest
                    | keyword == "release" -> KeywordRelease : lexer rest
                    | keyword == "rename" -> KeywordRename : lexer rest
                    | keyword == "replace" -> KeywordReplace : lexer rest
                    | keyword == "restrict" -> KeywordRestrict : lexer rest
                    | keyword == "right" -> KeywordRight : lexer rest
                    | keyword == "rollback" -> KeywordRollback : lexer rest
                    | keyword == "row" -> KeywordRow : lexer rest
                    | keyword == "savepoint" -> KeywordSavepoint : lexer rest
                    | keyword == "select" -> KeywordSelect : lexer rest
                    | keyword == "set" -> KeywordSet : lexer rest
                    | keyword == "table" -> KeywordTable : lexer rest
                    | keyword == "temp" -> KeywordTemp : lexer rest
                    | keyword == "temporary" -> KeywordTemporary : lexer rest
                    | keyword == "then" -> KeywordThen : lexer rest
                    | keyword == "to" -> KeywordTo : lexer rest
                    | keyword == "transaction" -> KeywordTransaction : lexer rest
                    | keyword == "trigger" -> KeywordTrigger : lexer rest
                    | keyword == "union" -> KeywordUnion : lexer rest
                    | keyword == "unique" -> KeywordUnique : lexer rest
                    | keyword == "update" -> KeywordUpdate : lexer rest
                    | keyword == "using" -> KeywordUsing : lexer rest
                    | keyword == "vacuum" -> KeywordVacuum : lexer rest
                    | keyword == "values" -> KeywordValues : lexer rest
                    | keyword == "view" -> KeywordView : lexer rest
                    | keyword == "virtual" -> KeywordVirtual : lexer rest
                    | keyword == "when" -> KeywordWhen : lexer rest
                    | keyword == "where" -> KeywordWhere : lexer rest
                    | otherwise -> (Identifier identifier) : lexer rest
  | isSpace c = lexer $ drop 1 all
  | otherwise = error $ "SQL-lexing error: Unexpected character '" ++ [c] ++ "'."


readStringLiteral :: String -> (Token, String)
readStringLiteral input =
    let readString' ('\'':('\'':rest)) = let (a, b) = readString' rest
                                         in ("'" ++ a, b)
        readString' ('\"':rest) = ("", rest)
        readString' (c:rest) = let (a, b) = readString' rest
                               in ([c] ++ a, b)
        (string, unparsed) = readString' $ drop 1 input
    in (LiteralString string, unparsed)


readNumericLiteral :: String -> (Token, String)
readNumericLiteral input =
    let (initialDigitSpan, restInitialDigitSpan) = span isDigit input
        (dotSpan, secondDigitSpan, restSecondDigitSpan)
            = if (length restInitialDigitSpan > 0) && (head restInitialDigitSpan == '.')
                then let (secondDigitSpan, restSecondDigitSpan)
                             = span isDigit $ tail restInitialDigitSpan
                     in (take 1 restInitialDigitSpan,
                         secondDigitSpan,
                         restSecondDigitSpan)
                else ("", "", restInitialDigitSpan)
        (exponentESpan, exponentSignSpan, exponentDigitSpan, restExponent)
            = if (length restSecondDigitSpan > 0)
                 && (toLower (head restSecondDigitSpan) == 'e')
                then let (exponentESpan, restE) = (take 1 restSecondDigitSpan,
                                                   drop 1 restSecondDigitSpan)
                         hasExponentSign
                             = (length restE > 0) && (elem (head restE) "+-")
                         (exponentSignSpan, restSign)
                             = if hasExponentSign
                                 then (take 1 $ drop 1 restE, drop 1 restE)
                                 else ("", restE)
                         (exponentDigitSpan, restExponent) = span isDigit restSign
                     in (exponentESpan,
                         exponentSignSpan,
                         exponentDigitSpan,
                         restExponent)
                else ("", "", "", restSecondDigitSpan)
        floatSpan = initialDigitSpan ++ dotSpan ++ secondDigitSpan
                    ++ exponentESpan ++ exponentSignSpan ++ exponentDigitSpan
        isFollowedByIdentifierCharacter
            = case restExponent of
                "" -> False
                (c:_) | (isAlphaNum c) || (elem c "_$") -> True
                      | otherwise -> False
        (trailingIdentifierSpan, restTrailingIdentifier)
            = span (\c -> (isAlphaNum c) || (elem c "_$")) restExponent
        errorSpan = floatSpan ++ trailingIdentifierSpan
        integerResult = let [(initialDigits, _)] = reads initialDigitSpan
                        in (LiteralInteger initialDigits, restExponent)
        floatResult = let tweakedFloatSpan = if not $ isDigit $ head floatSpan
                                               then "0" ++ floatSpan
                                               else floatSpan
                          [(double, _)] = reads tweakedFloatSpan
                      in (LiteralFloat $ fromJust $ mkNonnegativeDouble double,
                          restExponent)
        errorResult = (ErrorToken $ "Invalid number token: " ++ (show errorSpan),
                       restTrailingIdentifier)
    in case (initialDigitSpan,
             dotSpan,
             secondDigitSpan,
             exponentESpan,
             exponentSignSpan,
             exponentDigitSpan,
             isFollowedByIdentifierCharacter) of
         (_, _, _, _, _, _, True) -> errorResult
         ((_:_), "", "", "", "", "", _) -> integerResult
         (_, ".", _, _, _, _, _) -> floatResult
         (_, _, _, (_:_), _, (_:_), _) -> floatResult
         _ -> errorResult
        

readIdentifierOrKeyword :: String -> (String, String)
readIdentifierOrKeyword input
    = span (\c -> (isAlphaNum c) || (elem c "_$")) input

}
