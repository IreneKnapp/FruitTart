{
{-# LANGUAGE ExistentialQuantification #-}
module Network.FruitTart.Base.SQL.Syntax (
       					  readType,
       					  readSelect
					 )
    where

import Data.Char
import Data.Maybe
import Data.Word
import Numeric

import Network.FruitTart.Base.SQL.Types

}

%name parseType Type
%name parseSelect Select
{-
%name parseStatementList StatementList
%name parseStatement Statement
%name parseExpression Expression
%name parseFromClause FromClause
%name parseWhereClause WhereClause
%name parseGroupClause GroupClause
%name parseOrderClause OrderClause
%name parseLimitClause LimitClause
%name parseWhenClause WhenClause
%name parseConflictClause ConflictClause
%name parseForeignKeyClause ForeignKeyClause

-}

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

Type :: { Type }
    : UnqualifiedIdentifier
    { Type $1 Nothing }
    | UnqualifiedIdentifier '(' MaybeSign EitherNonnegativeDoubleWord64 ')'
    { Type $1 (Just (($3, $4), Nothing)) }
    | UnqualifiedIdentifier '(' MaybeSign EitherNonnegativeDoubleWord64 ','
      MaybeSign EitherNonnegativeDoubleWord64 ')'
    { Type $1 (Just (($3, $4), Just ($6, $7))) }

EitherNonnegativeDoubleWord64 :: { Either NonnegativeDouble Word64 }
    : float
    { Left $1 }
    | integer
    { Right $ 1}

LikeType :: { LikeType }
    : like
    { Like }
    | not like
    { NotLike }
    | glob
    { Glob }
    | not glob
    { NotGlob }
    | regexp
    { Regexp }
    | not regexp
    { NotRegexp }
    | match
    { Match }
    | not match
    { NotMatch }

Expression :: { Expression }
    : integer
    { ExpressionLiteralInteger $1 }
    | float
    { ExpressionLiteralFloat $1 }
    | string
    { ExpressionLiteralString $1 }
    | blob
    { ExpressionLiteralBlob $1 }
    | null
    { ExpressionLiteralNull }
    | currentTime
    { ExpressionLiteralCurrentTime }
    | currentDate
    { ExpressionLiteralCurrentDate }
    | currentTimestamp
    { ExpressionLiteralCurrentTimestamp }
    | variable
    { ExpressionVariable }
    | variableN
    { ExpressionVariableN $1 }
    | variableNamed
    { ExpressionVariableNamed $1 }
    | DoublyQualifiedIdentifier
    { ExpressionIdentifier $1 }
    | '-' Expression
    { ExpressionUnaryNegative $2 }
    | '+' Expression
    { ExpressionUnaryPositive $2 }
    | '~' Expression
    { ExpressionUnaryBitwiseNot $2 }
    | not Expression		
    { ExpressionUnaryLogicalNot $2 }
    | Expression '||' Expression
    { ExpressionBinaryConcatenate $1 $3 }
    | Expression '*' Expression
    { ExpressionBinaryMultiply $1 $3 }
    | Expression '/' Expression
    { ExpressionBinaryDivide $1 $3 }
    | Expression '%' Expression
    { ExpressionBinaryModulus $1 $3 }
    | Expression '+' Expression
    { ExpressionBinaryAdd $1 $3 }
    | Expression '-' Expression
    { ExpressionBinarySubtract $1 $3 }
    | Expression '<<' Expression
    { ExpressionBinaryLeftShift $1 $3 }
    | Expression '>>' Expression
    { ExpressionBinaryRightShift $1 $3 }
    | Expression '&' Expression
    { ExpressionBinaryBitwiseAnd $1 $3 }
    | Expression '|' Expression
    { ExpressionBinaryBitwiseOr $1 $3 }
    | Expression '<' Expression
    { ExpressionBinaryLess $1 $3 }
    | Expression '<=' Expression
    { ExpressionBinaryLessEquals $1 $3 }
    | Expression '>' Expression
    { ExpressionBinaryGreater $1 $3 }
    | Expression '>=' Expression
    { ExpressionBinaryGreaterEquals $1 $3 }
    | Expression '=' Expression
    { ExpressionBinaryEquals $1 $3 }
    | Expression '==' Expression
    { ExpressionBinaryEqualsEquals $1 $3 }
    | Expression '!=' Expression
    { ExpressionBinaryNotEquals $1 $3 }
    | Expression '<>' Expression
    { ExpressionBinaryLessGreater $1 $3 }
    | Expression and Expression
    { ExpressionBinaryLogicalAnd $1 $3 }
    | Expression or Expression
    { ExpressionBinaryLogicalOr $1 $3 }
    | UnqualifiedIdentifier '(' ExpressionList ')'
    { ExpressionFunctionCall $1 $3 }
    | UnqualifiedIdentifier '(' distinct OneOrMoreExpression ')'
    { ExpressionFunctionCallDistinct $1 (fromJust $ mkOneOrMore $4) }
    | UnqualifiedIdentifier '(' '*' ')'
    { ExpressionFunctionCallStar $1 }
    | cast '(' Expression as Type ')'
    { ExpressionCast $3 $5 }
    | Expression collate UnqualifiedIdentifier
    { ExpressionCollate $1 $3 }
    | Expression LikeType Expression
    { ExpressionLike $1 $2 $3 Nothing }
    | Expression LikeType Expression escape Expression
    { ExpressionLike $1 $2 $3 (Just $5) }
    | Expression isnull
    { ExpressionIsnull $1 }
    | Expression notnull
    { ExpressionNotnull $1 }
    | Expression not null
    { ExpressionNotNull $1 }
    | Expression is Expression
    { ExpressionIs $1 $3 }
    | Expression is not Expression
    { ExpressionIsNot $1 $4 }
    | Expression between Expression and Expression
    { ExpressionBetween $1 $3 $5 }
    | Expression not between Expression and Expression
    { ExpressionNotBetween $1 $4 $6 }
    | Expression in '(' Select ')'
    { ExpressionInSelect $1 $4 }
    | Expression not in '(' Select ')'
    { ExpressionNotInSelect $1 $5 }
    | Expression in '(' ExpressionList ')'
    { ExpressionInList $1 $4 }
    | Expression not in '(' ExpressionList ')'
    { ExpressionNotInList $1 $5 }
    | Expression in SinglyQualifiedIdentifier
    { ExpressionInTable $1 $3 }
    | Expression not in SinglyQualifiedIdentifier
    { ExpressionNotInTable $1 $4 }
    | '(' Select ')'
    { ExpressionSubquery $2 }
    | exists '(' Select ')'
    { ExpressionExistsSubquery $3 }
    | not exists '(' Select ')'
    { ExpressionNotExistsSubquery $4 }
    | case CaseList end
    { ExpressionCase Nothing (fromJust $ mkOneOrMore $2) Nothing }
    | case CaseList else Expression end
    { ExpressionCase Nothing (fromJust $ mkOneOrMore $2) (Just $4) }
    | case Expression CaseList end
    { ExpressionCase (Just $2) (fromJust $ mkOneOrMore $3) Nothing }
    | case Expression CaseList else Expression end
    { ExpressionCase (Just $2) (fromJust $ mkOneOrMore $3) (Just $5) }
    | raise '(' ignore ')'
    { ExpressionRaiseIgnore }
    | raise '(' rollback ',' string ')'
    { ExpressionRaiseRollback $5 }
    | raise '(' abort ',' string ')'
    { ExpressionRaiseAbort $5 }
    | raise '(' fail ',' string ')'
    { ExpressionRaiseFail $5 }

ExpressionList :: { [Expression] }
    :
    { [] }
    | ExpressionList Expression
    { $1 ++ [$2] }

OneOrMoreExpression :: { [Expression] }
    : Expression
    { [$1] }
    | ExpressionList ',' Expression
    { $1 ++ [$3] }

CaseList :: { [(Expression, Expression)] }
    : when Expression then Expression
    { [($2, $4)] }
    | CaseList when Expression then Expression
    { $1 ++ [($3, $5)] }

MaybeUnique :: { MaybeUnique }
    :
    { NoUnique }
    | unique
    { Unique }

MaybeIfNotExists :: { MaybeIfNotExists }
    :
    { NoIfNotExists }
    | if not exists
    { IfNotExists }

MaybeIfExists :: { MaybeIfExists }
    :
    { NoIfExists }
    | if exists
    { IfExists }

MaybeForEachRow :: { MaybeForEachRow }
    :
    { NoForEachRow }
    | for each row
    { ForEachRow }

Permanence :: { Permanence }
    :
    { Permanent }
    | temp
    { Temp }
    | temporary
    { Temporary }

MaybeCollation :: { MaybeCollation }
    :
    { NoCollation }
    | collate UnqualifiedIdentifier
    { Collation $2 }

MaybeAscDesc :: { MaybeAscDesc }
    :
    { NoAscDesc }
    | asc
    { Asc }
    | desc
    { Desc }

MaybeAutoincrement :: { MaybeAutoincrement }
    :
    { NoAutoincrement }
    | autoincrement
    { Autoincrement }

MaybeSign :: { MaybeSign }
    :
    { NoSign }
    | '+'
    { PositiveSign }
    | '-'
    { NegativeSign }

-- AlterTableBody
-- TODO definition

-- ColumnDefinition
-- TODO definition

-- DefaultValue
-- TODO definition

-- IndexedColumn
-- TODO definition

-- ColumnConstraint
-- TODO definition

-- TableConstraint
-- TODO definition

-- TriggerTime
-- TODO definition

-- TriggerCondition
-- TODO definition

-- ModuleArgument
-- TODO definition

-- TriggerStatement
-- TODO definition

-- QualifiedTableName
-- TODO definition

OrderingTerm :: { OrderingTerm }
    : Expression MaybeCollation MaybeAscDesc
    { OrderingTerm $1 $2 $3 }

OneOrMoreOrderingTerm :: { [OrderingTerm] }
    : OrderingTerm
    { [$1] }
    | OneOrMoreOrderingTerm ',' OrderingTerm
    { $1 ++ [$3] }

-- PragmaValue
-- TODO definition

-- PragmaValue'
-- TODO definition

-- InsertHead
-- TODO definition

-- InsertBody
-- TODO definition

-- UpdateHead
-- TODO definition

Distinctness :: { Distinctness }
    :
    { NoDistinctness }
    | distinct
    { Distinct }
    | all
    { All }

MaybeHaving :: { MaybeHaving }
    :
    { NoHaving }
    | having Expression
    { Having $2 }

MaybeAs :: { MaybeAs }
    :
    { NoAs }
    | as UnqualifiedIdentifier
    { As $2 }
    | UnqualifiedIdentifier
    { ElidedAs $1 }

CompoundOperator :: { CompoundOperator }
    : union
    { Union }
    | union all
    { UnionAll }
    | intersect
    { Intersect }
    | except
    { Except }

SelectCore :: { SelectCore }
    : select Distinctness OneOrMoreResultColumn MaybeFromClause MaybeWhereClause
      MaybeGroupClause
    { SelectCore $2 (fromJust $ mkOneOrMore $3) $4 $5 $6 }

SelectCoreList :: { [(CompoundOperator, SelectCore)] }
    :
    { [] }
    | SelectCoreList CompoundOperator SelectCore
    { $1 ++ [($2, $3)] }

ResultColumn :: { ResultColumn }
    : '*'
    { Star }
    | UnqualifiedIdentifier '.' '*'
    { TableStar $1 }
    | Expression MaybeAs
    { Result $1 $2 }

OneOrMoreResultColumn :: { [ResultColumn] }
    : ResultColumn
    { [$1] }
    | OneOrMoreResultColumn ',' ResultColumn
    { $1 ++ [$3] }

JoinSource :: { JoinSource }
    : SingleSource ListJoins
    { JoinSource $1 $2 }

ListJoins :: { [(JoinOperation, SingleSource, JoinConstraint)] }
    :
    { [] }
    | ListJoins JoinOperation SingleSource JoinConstraint
    { $1 ++ [($2, $3, $4)] }

SingleSource :: { SingleSource }
    : SinglyQualifiedIdentifier MaybeAs MaybeIndexedBy
    { TableSource $1 $2 $3 }
    | '(' Select ')' MaybeAs
    { SelectSource $2 $4 }
    | '(' JoinSource ')'
    { SubjoinSource $2 }

JoinOperation :: { JoinOperation }
    : ','
    { Comma }
    | join
    { Join }
    | outer join
    { OuterJoin }
    | left join
    { LeftJoin }
    | left outer join
    { LeftOuterJoin }
    | inner join
    { InnerJoin }
    | cross join
    { CrossJoin }
    | natural join
    { NaturalJoin }
    | natural outer join
    { NaturalOuterJoin }
    | natural left join
    { NaturalLeftJoin }
    | natural left outer join
    { NaturalLeftOuterJoin }
    | natural inner join
    { NaturalInnerJoin }
    | natural cross join
    { NaturalCrossJoin }

JoinConstraint :: { JoinConstraint }
    :
    { NoConstraint }
    | on Expression
    { On $2 }
    | using '(' OneOrMoreUnqualifiedIdentifier ')'
    { Using (fromJust $ mkOneOrMore $3) }

MaybeIndexedBy :: { MaybeIndexedBy }
    :
    { NoIndexedBy }
    | indexed by UnqualifiedIdentifier
    { IndexedBy $3 }
    | not indexed
    { NotIndexed }

FromClause :: { FromClause }
    : from JoinSource
    { From $2 }

MaybeFromClause :: { Maybe FromClause }
    :
    { Nothing }
    | FromClause
    { Just $1 }

WhereClause :: { WhereClause }
    : where Expression
    { Where $2 }

MaybeWhereClause :: { Maybe WhereClause }
    :
    { Nothing }
    | WhereClause
    { Just $1 }

GroupClause :: { GroupClause }
    : group by OneOrMoreOrderingTerm MaybeHaving
    { GroupBy (fromJust $ mkOneOrMore $3) $4 }

MaybeGroupClause :: { Maybe GroupClause }
    :
    { Nothing }
    | GroupClause
    { Just $1 }

OrderClause :: { OrderClause }
    : order by OneOrMoreOrderingTerm
    { OrderBy (fromJust $ mkOneOrMore $3) }

MaybeOrderClause :: { Maybe OrderClause }
    :
    { Nothing }
    | OrderClause
    { Just $1 }

LimitClause :: { LimitClause }
    : limit integer
    { Limit $2 }
    | limit integer offset integer
    { LimitOffset $2 $4 }
    | limit integer ',' integer
    { LimitComma $2 $4 }

MaybeLimitClause :: { Maybe LimitClause }
    :
    { Nothing }
    | LimitClause
    { Just $1 }

-- WhenClause
-- TODO definition

-- ConflictClause
-- TODO definition

-- ForeignKeyClause
-- TODO definition

-- ForeignKeyClauseActionOrMatchPart
-- TODO definition

-- ForeignKeyClauseActionPart
-- TODO definition

-- ForeignKeyClauseDeferrablePart
-- TODO definition

-- MaybeInitialDeferralStatus
-- TODO definition

-- MaybeTransactionType
-- TODO definition

-- StatementList
-- TODO definition

-- AnyStatement
-- TODO definition

-- Statement
-- TODO definition

-- Explain
-- TODO definition

-- ExplainQueryPlan
-- TODO definition

-- AlterTable
-- TODO definition

-- Analyze
-- TODO definition

-- Attach
-- TODO definition

-- Begin
-- TODO definition

-- Commit
-- TODO definition

-- CreateIndex
-- TODO definition

-- CreateTable
-- TODO definition

-- CreateTrigger
-- TODO definition

-- CreateView
-- TODO definition

-- CreateVirtualTable
-- TODO definition

-- Delete
-- TODO definition

-- DeleteLimited
-- TODO definition

-- Detach
-- TODO definition

-- DropIndex
-- TODO definition

-- DropTable
-- TODO definition

-- DropTrigger
-- TODO definition

-- DropView
-- TODO definition

-- Insert
-- TODO definition

-- Pragma
-- TODO definition

-- Reindex
-- TODO definition

-- Release
-- TODO definition

-- Rollback
-- TODO definition

-- Savepoint
-- TODO definition

Select :: { Select }
    : SelectCore SelectCoreList MaybeOrderClause MaybeLimitClause
    { Select $1 $2 $3 $4 }

-- Update
-- TODO definition

-- UpdateLimited
-- TODO definition

-- Vacuum
-- TODO definition

UnqualifiedIdentifier :: { UnqualifiedIdentifier }
    : identifier
    { UnqualifiedIdentifier $1 }

OneOrMoreUnqualifiedIdentifier :: { [UnqualifiedIdentifier] }
    : UnqualifiedIdentifier
    { [$1] }
    | OneOrMoreUnqualifiedIdentifier ',' UnqualifiedIdentifier
    { $1 ++ [$3] }

SinglyQualifiedIdentifier :: { SinglyQualifiedIdentifier }
    : identifier
    { SinglyQualifiedIdentifier Nothing $1 }
    | identifier '.' identifier
    { SinglyQualifiedIdentifier (Just $1) $3 }

DoublyQualifiedIdentifier :: { DoublyQualifiedIdentifier }
    : identifier
    { DoublyQualifiedIdentifier Nothing $1 }
    | identifier '.' identifier
    { DoublyQualifiedIdentifier (Just ($1, Nothing)) $3 }
    | identifier '.' identifier '.' identifier
    { DoublyQualifiedIdentifier (Just ($3, Just $1)) $5 }

{

readType :: String -> Type
readType input = parseType $ lexer input


readSelect :: String -> Select
readSelect input = parseSelect $ lexer input


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
