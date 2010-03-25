{
{-# LANGUAGE ExistentialQuantification, Rank2Types #-}
module Network.FruitTart.Base.SQL.Syntax (
                                          readType,
                                          readLikeType,
                                          readExpression,
                                          readMaybeUnique,
                                          readMaybeIfNotExists,
                                          readMaybeIfExists,
                                          readMaybeForEachRow,
                                          readPermanence,
                                          readMaybeCollation,
                                          readMaybeAscDesc,
                                          readMaybeAutoincrement,
                                          readMaybeSign,
                                          readAlterTableBody,
                                          readColumnDefinition,
                                          readDefaultValue,
                                          readIndexedColumn,
                                          readColumnConstraint,
                                          readTableConstraint,
                                          readTriggerTime,
                                          readTriggerCondition,
                                          -- TODO remember to uncomment this
                                          -- readModuleArgument,
                                          readTriggerStatement,
                                          readQualifiedTableName,
                                          readOrderingTerm,
                                          readPragmaBody,
                                          readPragmaValue,
                                          readEitherColumnsAndConstraintsSelect,
                                          readInsertHead,
                                          readInsertBody,
                                          readUpdateHead,
                                          readDistinctness,
                                          readMaybeHaving,
                                          readMaybeAs,
                                          readCompoundOperator,
                                          readSelectCore,
                                          readResultColumn,
                                          readJoinSource,
                                          readSingleSource,
                                          readJoinOperation,
                                          readJoinConstraint,
                                          readMaybeIndexedBy,
                                          readFromClause,
                                          readWhereClause,
                                          readGroupClause,
                                          readOrderClause,
                                          readLimitClause,
                                          readWhenClause,
                                          readConflictClause,
                                          readForeignKeyClause,
                                          readForeignKeyClauseActionOrMatchPart,
                                          readForeignKeyClauseActionPart,
                                          readForeignKeyClauseDeferrablePart,
                                          readMaybeInitialDeferralStatus,
                                          readMaybeTransactionType,
                                          readStatementList,
                                          readAnyStatement,
                                          readExplainableStatement,
                                          readExplain,
                                          readExplainQueryPlan,
                                          readAlterTable,
                                          readAnalyze,
                                          readAttach,
                                          readBegin,
                                          readCommit,
                                          readCreateIndex,
                                          readCreateTable,
                                          readCreateTrigger,
                                          readCreateView,
                                          -- TODO remember to uncomment this
                                          -- readCreateVirtualTable,
                                          readDelete,
                                          readDeleteLimited,
                                          readDeleteOrDeleteLimited,
                                          readDetach,
                                          readDropIndex,
                                          readDropTable,
                                          readDropTrigger,
                                          readDropView,
                                          readInsert,
                                          readPragma,
                                          readReindex,
                                          readRelease,
                                          readRollback,
                                          readSavepoint,
                                          readSelect,
                                          readUpdate,
                                          readUpdateLimited,
                                          readUpdateOrUpdateLimited,
                                          readVacuum,
                                          readUnqualifiedIdentifier,
                                          readSinglyQualifiedIdentifier,
                                          readDoublyQualifiedIdentifier
                                         )
    where

import Data.Char
import Data.Maybe
import Data.Word
import Numeric

import Network.FruitTart.Base.SQL.Types

}

%name parseType Type
%name parseLikeType LikeType
%name parseExpression Expression
%name parseMaybeUnique MaybeUnique
%name parseMaybeIfNotExists MaybeIfNotExists
%name parseMaybeIfExists MaybeIfExists
%name parseMaybeForEachRow MaybeForEachRow
%name parsePermanence Permanence
%name parseMaybeCollation MaybeCollation
%name parseMaybeAscDesc MaybeAscDesc
%name parseMaybeAutoincrement MaybeAutoincrement
%name parseMaybeSign MaybeSign
%name parseAlterTableBody AlterTableBody
%name parseColumnDefinition ColumnDefinition
%name parseDefaultValue DefaultValue
%name parseIndexedColumn IndexedColumn
%name parseColumnConstraint ColumnConstraint
%name parseTableConstraint TableConstraint
%name parseTriggerTime TriggerTime
%name parseTriggerCondition TriggerCondition
-- %name parseModuleArgument ModuleArgument
-- TODO remember to uncomment this
%name parseTriggerStatement TriggerStatement
%name parseQualifiedTableName QualifiedTableName
%name parseOrderingTerm OrderingTerm
%name parsePragmaBody PragmaBody
%name parsePragmaValue PragmaValue
%name parseEitherColumnsAndConstraintsSelect EitherColumnsAndConstraintsSelect
%name parseInsertHead InsertHead
%name parseInsertBody InsertBody
%name parseUpdateHead UpdateHead
%name parseDistinctness Distinctness
%name parseMaybeHaving MaybeHaving
%name parseMaybeAs MaybeAs
%name parseCompoundOperator CompoundOperator
%name parseSelectCore SelectCore
%name parseResultColumn ResultColumn
%name parseJoinSource JoinSource
%name parseSingleSource SingleSource
%name parseJoinOperation JoinOperation
%name parseJoinConstraint JoinConstraint
%name parseMaybeIndexedBy MaybeIndexedBy
%name parseFromClause FromClause
%name parseWhereClause WhereClause
%name parseGroupClause GroupClause
%name parseOrderClause OrderClause
%name parseLimitClause LimitClause
%name parseWhenClause WhenClause
%name parseConflictClause ConflictClause
%name parseForeignKeyClause ForeignKeyClause
%name parseForeignKeyClauseActionOrMatchPart ForeignKeyClauseActionOrMatchPart
%name parseForeignKeyClauseActionPart ForeignKeyClauseActionPart
%name parseForeignKeyClauseDeferrablePart ForeignKeyClauseDeferrablePart
%name parseMaybeInitialDeferralStatus MaybeInitialDeferralStatus
%name parseMaybeTransactionType MaybeTransactionType
%name parseStatementList StatementList
%name parseAnyStatement Statement
%name parseExplainableStatement ExplainableStatement
%name parseExplain Explain
%name parseExplainQueryPlan ExplainQueryPlan
%name parseAlterTable AlterTable
%name parseAnalyze Analyze
%name parseAttach Attach
%name parseBegin Begin
%name parseCommit Commit
%name parseCreateIndex CreateIndex
%name parseCreateTable CreateTable
%name parseCreateTrigger CreateTrigger
%name parseCreateView CreateView
-- %name parseCreateVirtualTable CreateVirtualTable
-- TODO remember to uncomment this
%name parseDelete Delete
%name parseDeleteLimited DeleteLimited
%name parseDeleteOrDeleteLimited DeleteOrDeleteLimited
%name parseDetach Detach
%name parseDropIndex DropIndex
%name parseDropTable DropTable
%name parseDropTrigger DropTrigger
%name parseDropView DropView
%name parseInsert Insert
%name parsePragma Pragma
%name parseReindex Reindex
%name parseRelease Release
%name parseRollback Rollback
%name parseSavepoint Savepoint
%name parseSelect Select
%name parseUpdate Update
%name parseUpdateLimited UpdateLimited
%name parseUpdateOrUpdateLimited UpdateOrUpdateLimited
%name parseVacuum Vacuum
%name parseUnqualifiedIdentifier UnqualifiedIdentifier
%name parseSinglyQualifiedIdentifier SinglyQualifiedIdentifier
%name parseDoublyQualifiedIdentifier DoublyQualifiedIdentifier

%tokentype { Token }
%error { parseError }

%left or
%left and
%left LOOSER_THAN_NOT
%right not
%left is match like glob regexp between in isnull notnull '!=' '<>' '=' '=='
%left '>' '>=' '<' '<='
%right escape
%left '&' '|' '<<' '>>'
%left '+' '-'
%left '*' '/' '%'
%left '|'
%left LOOSER_THAN_COLLATE
%left collate
%right '~'

%left LOOSER_THAN_DOT
%left '.'

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
        abort                 { KeywordAbort }
        action                { KeywordAction }
        add                   { KeywordAdd }
        after                 { KeywordAfter }
        all                   { KeywordAll }
        alter                 { KeywordAlter }
        analyze               { KeywordAnalyze }
        and                   { KeywordAnd }
        as                    { KeywordAs }
        asc                   { KeywordAsc }
        attach                { KeywordAttach }
        autoincrement         { KeywordAutoincrement }
        before                { KeywordBefore }
        begin                 { KeywordBegin }
        between               { KeywordBetween }
        by                    { KeywordBy }
        cascade               { KeywordCascade }
        case                  { KeywordCase }
        cast                  { KeywordCast }
        check                 { KeywordCheck }
        collate               { KeywordCollate }
        column                { KeywordColumn }
        commit                { KeywordCommit }
        conflict              { KeywordConflict }
        constraint            { KeywordConstraint }
        create                { KeywordCreate }
        cross                 { KeywordCross }
        currentDate           { KeywordCurrentDate }
        currentTime           { KeywordCurrentTime }
        currentTimestamp      { KeywordCurrentTimestamp }
        database              { KeywordDatabase }
        default               { KeywordDefault }
        deferrable            { KeywordDeferrable }
        deferred              { KeywordDeferred }
        delete                { KeywordDelete }
        desc                  { KeywordDesc }
        detach                { KeywordDetach }
        distinct              { KeywordDistinct }
        drop                  { KeywordDrop }
        each                  { KeywordEach }
        else                  { KeywordElse }
        end                   { KeywordEnd }
        escape                { KeywordEscape }
        except                { KeywordExcept }
        exclusive             { KeywordExclusive }
        exists                { KeywordExists }
        explain               { KeywordExplain }
        fail                  { KeywordFail }
        for                   { KeywordFor }
        foreign               { KeywordForeign }
        from                  { KeywordFrom }
        full                  { KeywordFull }
        glob                  { KeywordGlob }
        group                 { KeywordGroup }
        having                { KeywordHaving }
        if                    { KeywordIf }
        ignore                { KeywordIgnore }
        immediate             { KeywordImmediate }
        in                    { KeywordIn }
        index                 { KeywordIndex }
        indexed               { KeywordIndexed }
        initially             { KeywordInitially }
        inner                 { KeywordInner }
        insert                { KeywordInsert }
        instead               { KeywordInstead }
        intersect             { KeywordIntersect }
        into                  { KeywordInto }
        is                    { KeywordIs }
        isnull                { KeywordIsnull }
        join                  { KeywordJoin }
        key                   { KeywordKey }
        left                  { KeywordLeft }
        like                  { KeywordLike }
        limit                 { KeywordLimit }
        match                 { KeywordMatch }
        natural               { KeywordNatural }
        no                    { KeywordNo }
        not                   { KeywordNot }
        notnull               { KeywordNotnull }
        null                  { KeywordNull }
        of                    { KeywordOf }
        offset                { KeywordOffset }
        on                    { KeywordOn }
        or                    { KeywordOr }
        order                 { KeywordOrder }
        outer                 { KeywordOuter }
        plan                  { KeywordPlan }
        pragma                { KeywordPragma }
        primary               { KeywordPrimary }
        query                 { KeywordQuery }
        raise                 { KeywordRaise }
        references            { KeywordReferences }
        regexp                { KeywordRegexp }
        reindex               { KeywordReindex }
        release               { KeywordRelease }
        rename                { KeywordRename }
        replace               { KeywordReplace }
        restrict              { KeywordRestrict }
        right                 { KeywordRight }
        rollback              { KeywordRollback }
        row                   { KeywordRow }
        savepoint             { KeywordSavepoint }
        select                { KeywordSelect }
        set                   { KeywordSet }
        table                 { KeywordTable }
        temp                  { KeywordTemp }
        temporary             { KeywordTemporary }
        then                  { KeywordThen }
        to                    { KeywordTo }
        transaction           { KeywordTransaction }
        trigger               { KeywordTrigger }
        union                 { KeywordUnion }
        unique                { KeywordUnique }
        update                { KeywordUpdate }
        using                 { KeywordUsing }
        vacuum                { KeywordVacuum }
        values                { KeywordValues }
        view                  { KeywordView }
        virtual               { KeywordVirtual }
        when                  { KeywordWhen }
        where                 { KeywordWhere }

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

Expression0 :: { Expression }
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
    | UnqualifiedIdentifier '(' ExpressionList ')'
    { ExpressionFunctionCall $1 $3 }
    | UnqualifiedIdentifier '(' distinct OneOrMoreExpression ')'
    { ExpressionFunctionCallDistinct $1 (fromJust $ mkOneOrMore $4) }
    | UnqualifiedIdentifier '(' '*' ')'
    { ExpressionFunctionCallStar $1 }
    | raise '(' ignore ')'
    { ExpressionRaiseIgnore }
    | raise '(' rollback ',' string ')'
    { ExpressionRaiseRollback $5 }
    | raise '(' abort ',' string ')'
    { ExpressionRaiseAbort $5 }
    | raise '(' fail ',' string ')'
    { ExpressionRaiseFail $5 }
    | '(' Expression ')'
    { ExpressionParenthesized $2 }

Expression1 :: { Expression }
    : Expression0
    { $1 }
    | cast '(' Expression as Type ')'
    { ExpressionCast $3 $5 }

Expression2 :: { Expression }
    : Expression1
    { $1 }
    | Expression2 collate UnqualifiedIdentifier
    { ExpressionCollate $1 $3 }

Expression3 :: { Expression }
    : Expression2 %prec LOOSER_THAN_COLLATE
    { $1 }
    | case CaseList end
    { ExpressionCase Nothing (fromJust $ mkOneOrMore $2) Nothing }
    | case CaseList else Expression end
    { ExpressionCase Nothing (fromJust $ mkOneOrMore $2) (Just $4) }
    | case Expression CaseList end
    { ExpressionCase (Just $2) (fromJust $ mkOneOrMore $3) Nothing }
    | case Expression CaseList else Expression end
    { ExpressionCase (Just $2) (fromJust $ mkOneOrMore $3) (Just $5) }

Expression4 :: { Expression }
    : Expression3
    { $1 }
    | exists '(' Select ')'
    { ExpressionExistsSubquery $3 }

Expression5 :: { Expression }
    : Expression4
    { $1 }
    | Expression5 in '(' Select ')'
    { ExpressionInSelect $1 $4 }
    | Expression5 not in '(' Select ')'
    { ExpressionNotInSelect $1 $5 }
    | Expression5 in SinglyQualifiedIdentifier
    { ExpressionInTable $1 $3 }
    | Expression5 not in SinglyQualifiedIdentifier
    { ExpressionNotInTable $1 $4 }

Expression6 :: { Expression }
    : Expression5 %prec LOOSER_THAN_NOT
    { $1 }
    | '(' Select ')'
    { ExpressionSubquery $2 }

Expression7 :: { Expression }
    : Expression6
    { $1 }
    | Expression7 between Expression17 and Expression6
    { ExpressionBetween $1 $3 $5 }
    | Expression7 not between Expression17 and Expression6
    { ExpressionNotBetween $1 $4 $6 }

Expression8 :: { Expression }
    : Expression7 %prec LOOSER_THAN_NOT
    { $1 }
    | '-' Expression8
    { ExpressionUnaryNegative $2 }
    | '+' Expression8
    { ExpressionUnaryPositive $2 }
    | '~' Expression8
    { ExpressionUnaryBitwiseNot $2 }
    | not Expression8
    { case $2 of
        ExpressionExistsSubquery subquery -> ExpressionNotExistsSubquery subquery
        subexpression -> ExpressionUnaryLogicalNot subexpression }

Expression9 :: { Expression }
    : Expression8
    { $1 }
    | Expression9 is Expression8
    { case $3 of
        ExpressionUnaryLogicalNot subexpression -> ExpressionIsNot $1 subexpression
        _ -> ExpressionIs $1 $3 }

Expression10 :: { Expression }
    : Expression9
    { $1 }
    | Expression10 isnull
    { ExpressionIsnull $1 }
    | Expression10 notnull
    { ExpressionNotnull $1 }
    | Expression10 not null
    { ExpressionNotNull $1 }

Expression11 :: { Expression }
    : Expression10 %prec LOOSER_THAN_NOT
    { $1 }
    | Expression11 LikeType Expression10 %prec LOOSER_THAN_NOT
    { ExpressionLike $1 $2 $3 Nothing }
    | Expression11 LikeType Expression10 escape Expression10 %prec LOOSER_THAN_NOT
    { ExpressionLike $1 $2 $3 (Just $5) }

Expression12 :: { Expression }
    : Expression11 %prec LOOSER_THAN_NOT
    { $1 }
    | Expression12 '||' Expression11
    { ExpressionBinaryConcatenate $1 $3 }

Expression13 :: { Expression }
    : Expression12
    { $1 }
    | Expression13 '*' Expression12
    { ExpressionBinaryMultiply $1 $3 }
    | Expression13 '/' Expression12
    { ExpressionBinaryDivide $1 $3 }
    | Expression13 '%' Expression12
    { ExpressionBinaryModulus $1 $3 }

Expression14 :: { Expression }
    : Expression13
    { $1 }
    | Expression14 '+' Expression13
    { ExpressionBinaryAdd $1 $3 }
    | Expression14 '-' Expression13
    { ExpressionBinarySubtract $1 $3 }

Expression15 :: { Expression }
    : Expression14
    { $1 }
    | Expression15 '<<' Expression14
    { ExpressionBinaryLeftShift $1 $3 }
    | Expression15 '>>' Expression14
    { ExpressionBinaryRightShift $1 $3 }
    | Expression15 '&' Expression14
    { ExpressionBinaryBitwiseAnd $1 $3 }
    | Expression15 '|' Expression14
    { ExpressionBinaryBitwiseOr $1 $3 }

Expression16 :: { Expression }
    : Expression15
    { $1 }
    | Expression16 '<' Expression15
    { ExpressionBinaryLess $1 $3 }
    | Expression16 '<=' Expression15
    { ExpressionBinaryLessEquals $1 $3 }
    | Expression16 '>' Expression15
    { ExpressionBinaryGreater $1 $3 }
    | Expression16 '>=' Expression15
    { ExpressionBinaryGreaterEquals $1 $3 }

Expression17 :: { Expression }
    : Expression16
    { $1 }
    | Expression17 '=' Expression16
    { ExpressionBinaryEquals $1 $3 }
    | Expression17 '==' Expression16
    { ExpressionBinaryEqualsEquals $1 $3 }
    | Expression17 '!=' Expression16
    { ExpressionBinaryNotEquals $1 $3 }
    | Expression17 '<>' Expression16
    { ExpressionBinaryLessGreater $1 $3 }
    | Expression17 in '(' ExpressionList ')'
    { ExpressionInList $1 $4 }
    | Expression17 not in '(' ExpressionList ')'
    { ExpressionNotInList $1 $5 }

Expression18 :: { Expression }
    : Expression17 %prec LOOSER_THAN_NOT
    { $1 }
    | Expression18 and Expression17
    { ExpressionBinaryLogicalAnd $1 $3 }

Expression19 :: { Expression }
    : Expression18
    { $1 }
    | Expression19 or Expression18
    { ExpressionBinaryLogicalOr $1 $3 }

Expression :: { Expression }
    : Expression19
    { $1 }

ExpressionList :: { [Expression] }
    :
    { [] }
    | ExpressionList Expression
    { $1 ++ [$2] }

OneOrMoreExpression :: { [Expression] }
    : Expression
    { [$1] }
    | OneOrMoreExpression ',' Expression
    { $1 ++ [$3] }

OneOrMoreSetPair :: { [(UnqualifiedIdentifier, Expression)] }
    : UnqualifiedIdentifier '=' Expression
    { [($1, $3)] }
    | OneOrMoreSetPair ',' UnqualifiedIdentifier '=' Expression
    { $1 ++ [($3, $5)] }

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

AlterTableBody :: { AlterTableBody }
    : rename to UnqualifiedIdentifier
    { RenameTo $3 }
    | add ColumnDefinition
    { AddColumn False $2 }
    | add column ColumnDefinition
    { AddColumn True $3 }

ColumnDefinition :: { ColumnDefinition }
    : UnqualifiedIdentifier ColumnConstraintList
    { ColumnDefinition $1 Nothing $2 }
    | UnqualifiedIdentifier Type ColumnConstraintList
    { ColumnDefinition $1 (Just $2) $3 }

OneOrMoreColumnDefinition :: { [ColumnDefinition] }
    : ColumnDefinition
    { [$1] }
    | OneOrMoreColumnDefinition ',' ColumnDefinition
    { $1 ++ [$3] }

DefaultValue :: { DefaultValue }
    : MaybeSign integer
    { DefaultValueSignedInteger $1 $2 }
    | MaybeSign float
    { DefaultValueSignedFloat $1 $2 }
    | string
    { DefaultValueLiteralString $1 }
    | blob
    { DefaultValueLiteralBlob $1 }
    | null
    { DefaultValueLiteralNull }
    | currentTime
    { DefaultValueLiteralCurrentTime }
    | currentDate
    { DefaultValueLiteralCurrentDate }
    | currentTimestamp
    { DefaultValueLiteralCurrentTimestamp }
    | '(' Expression ')'
    { DefaultValueExpression $2 }

IndexedColumn :: { IndexedColumn }
    : UnqualifiedIdentifier MaybeCollation MaybeAscDesc
    { IndexedColumn $1 $2 $3 }

OneOrMoreIndexedColumn :: { [IndexedColumn] }
    : IndexedColumn
    { [$1] }
    | OneOrMoreIndexedColumn ',' IndexedColumn
    { $1 ++ [$3] }

ColumnConstraint :: { ColumnConstraint }
    : constraint UnqualifiedIdentifier primary key MaybeAscDesc MaybeConflictClause
      MaybeAutoincrement
    { ColumnPrimaryKey $2 $5 $6 $7 }
    | constraint UnqualifiedIdentifier not null MaybeConflictClause
    { ColumnNotNull $2 $5 }
    | constraint UnqualifiedIdentifier unique MaybeConflictClause
    { ColumnUnique $2 $4 }
    | constraint UnqualifiedIdentifier check '(' Expression ')'
    { ColumnCheck $2 $5 }
    | constraint UnqualifiedIdentifier default DefaultValue
    { ColumnDefault $2 $4 }
    | constraint UnqualifiedIdentifier collate UnqualifiedIdentifier
    { ColumnCollate $2 $4 }
    | constraint UnqualifiedIdentifier ForeignKeyClause
    { ColumnForeignKey $2 $3 }

ColumnConstraintList :: { [ColumnConstraint] }
    :
    { [] }
    | ColumnConstraintList ColumnConstraint
    { $1 ++ [$2] }

TableConstraint :: { TableConstraint }
    : constraint UnqualifiedIdentifier primary key '(' OneOrMoreIndexedColumn ')'
      MaybeConflictClause
    { TablePrimaryKey $2 (fromJust $ mkOneOrMore $6) $8 }
    | constraint UnqualifiedIdentifier unique '(' OneOrMoreIndexedColumn ')'
      MaybeConflictClause
    { TableUnique $2 (fromJust $ mkOneOrMore $5) $7 }
    | constraint UnqualifiedIdentifier check '(' Expression ')'
    { TableCheck $2 $5 }
    | constraint UnqualifiedIdentifier foreign key '(' OneOrMoreUnqualifiedIdentifier ')'
      ForeignKeyClause
    { TableForeignKey $2 (fromJust $ mkOneOrMore $6) $8 }

OneOrMoreTableConstraint :: { [TableConstraint] }
    : TableConstraint
    { [$1] }
    | OneOrMoreTableConstraint ',' TableConstraint
    { $1 ++ [$3] }

EitherColumnsAndConstraintsSelect :: { EitherColumnsAndConstraintsSelect }
    : '(' OneOrMoreColumnDefinition ')'
    { ColumnsAndConstraints (fromJust $ mkOneOrMore $2) [] }
    | '(' OneOrMoreColumnDefinition ',' OneOrMoreTableConstraint ')'
    { ColumnsAndConstraints (fromJust $ mkOneOrMore $2) $4 }
    | as Select
    { AsSelect $2 }

TriggerTime :: { TriggerTime }
    : before
    { Before }
    | after
    { After }
    | instead of
    { InsteadOf }

TriggerCondition :: { TriggerCondition }
    : delete on
    { DeleteOn }
    | insert on
    { InsertOn }
    | update on
    { UpdateOn [] }
    | update of OneOrMoreUnqualifiedIdentifier on
    { UpdateOn $3 }

-- ModuleArgument :: { ModuleArgument }
--     :
--     { }
-- TODO definition (requires monadic parser)

TriggerStatement :: { TriggerStatement }
    : Update
    { TriggerStatement $1 }
    | Insert
    { TriggerStatement $1 }
    | Delete
    { TriggerStatement $1 }
    | Select
    { TriggerStatement $1 }

OneOrMoreTriggerStatement :: { [TriggerStatement] }
    : TriggerStatement
    { [$1] }
    | OneOrMoreTriggerStatement ';' TriggerStatement
    { $1 ++ [$3] }

QualifiedTableName :: { QualifiedTableName }
    : SinglyQualifiedIdentifier
    { TableNoIndexedBy $1 }
    | SinglyQualifiedIdentifier indexed by UnqualifiedIdentifier
    { TableIndexedBy $1 $4 }
    | SinglyQualifiedIdentifier not indexed
    { TableNotIndexed $1 }

OrderingTerm :: { OrderingTerm }
    : Expression MaybeCollation MaybeAscDesc
    { OrderingTerm $1 $2 $3 }

OneOrMoreOrderingTerm :: { [OrderingTerm] }
    : OrderingTerm
    { [$1] }
    | OneOrMoreOrderingTerm ',' OrderingTerm
    { $1 ++ [$3] }

PragmaBody :: { PragmaBody }
    :
    { EmptyPragmaBody }
    | '=' PragmaValue
    { EqualsPragmaBody $2 }
    | '(' PragmaValue ')'
    { CallPragmaBody $2 }

PragmaValue :: { PragmaValue }
    : MaybeSign integer
    { SignedIntegerPragmaValue $1 $2 }
    | MaybeSign float
    { SignedFloatPragmaValue $1 $2 }
    | UnqualifiedIdentifier
    { NamePragmaValue $1 }
    | string
    { StringPragmaValue $1 }

InsertHead :: { InsertHead }
    : insert
    { InsertNoAlternative }
    | insert or rollback
    { InsertOrRollback }
    | insert or abort
    { InsertOrAbort }
    | insert or replace
    { InsertOrReplace }
    | insert or fail
    { InsertOrFail }
    | insert or ignore
    { InsertOrIgnore }
    | replace
    { Replace }

InsertBody :: { InsertBody }
    : values '(' OneOrMoreExpression ')'
    { InsertValues [] (fromJust $ mkOneOrMore $3) }
    | '(' OneOrMoreUnqualifiedIdentifier ')' values '(' OneOrMoreExpression ')'
    { InsertValues $2 (fromJust $ mkOneOrMore $6) }
    | Select
    { InsertSelect [] $1 }
    | '(' OneOrMoreUnqualifiedIdentifier ')' Select
    { InsertSelect $2 $4 }
    | default values
    { InsertDefaultValues }

UpdateHead :: { UpdateHead }
    : update
    { UpdateNoAlternative }
    | update or rollback
    { UpdateOrRollback }
    | update or abort
    { UpdateOrAbort }
    | update or replace
    { UpdateOrReplace }
    | update or fail
    { UpdateOrFail }
    | update or ignore
    { UpdateOrIgnore }

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

WhenClause :: { WhenClause }
    : when Expression
    { When $2 }

MaybeWhenClause :: { Maybe WhenClause }
    :
    { Nothing }
    | WhenClause
    { Just $1 }

ConflictClause :: { ConflictClause }
    : on conflict rollback
    { OnConflictRollback }
    | on conflict abort
    { OnConflictAbort }
    | on conflict fail
    { OnConflictFail }
    | on conflict ignore
    { OnConflictIgnore }
    | on conflict replace
    { OnConflictReplace }

MaybeConflictClause :: { Maybe ConflictClause }
    :
    { Nothing }
    | ConflictClause
    { Just $1 }

ForeignKeyClause :: { ForeignKeyClause }
    : references UnqualifiedIdentifier ForeignKeyClauseActionOrMatchPartList
      MaybeForeignKeyClauseDeferrablePart
    { References $2 [] $3 $4 }
    | references UnqualifiedIdentifier '(' OneOrMoreUnqualifiedIdentifier ')'
      ForeignKeyClauseActionOrMatchPartList MaybeForeignKeyClauseDeferrablePart
    { References $2 $4 $6 $7 }

ForeignKeyClauseActionOrMatchPart :: { ForeignKeyClauseActionOrMatchPart }
    : on delete ForeignKeyClauseActionPart
    { OnDelete $3 }
    | on update ForeignKeyClauseActionPart
    { OnUpdate $3 }
    | match UnqualifiedIdentifier
    { ReferencesMatch $2 }

ForeignKeyClauseActionOrMatchPartList :: { [ForeignKeyClauseActionOrMatchPart] }
    :
    { [] }
    | ForeignKeyClauseActionOrMatchPartList ForeignKeyClauseActionOrMatchPart
    { $1 ++ [$2] }

ForeignKeyClauseActionPart :: { ForeignKeyClauseActionPart }
    : set null
    { SetNull }
    | set default
    { SetDefault }
    | cascade
    { Cascade }
    | restrict
    { Restrict }
    | no action
    { NoAction }

ForeignKeyClauseDeferrablePart :: { ForeignKeyClauseDeferrablePart }
    : deferrable MaybeInitialDeferralStatus
    { Deferrable $2 }
    | not deferrable MaybeInitialDeferralStatus
    { NotDeferrable $3 }

MaybeForeignKeyClauseDeferrablePart :: { Maybe ForeignKeyClauseDeferrablePart }
    :
    { Nothing }
    | ForeignKeyClauseDeferrablePart
    { Just $1 }

MaybeInitialDeferralStatus :: { MaybeInitialDeferralStatus }
    :
    { NoInitialDeferralStatus }
    | initially deferred
    { InitiallyDeferred }
    | initially immediate
    { InitiallyImmediate }

MaybeTransactionType :: { MaybeTransactionType }
    :
    { NoTransactionType }
    | deferred
    { Deferred }
    | immediate
    { Immediate }
    | exclusive
    { Exclusive }

StatementList :: { StatementList }
    :
    { StatementList [] }
    | OneOrMoreStatement
    { StatementList $1 }

OneOrMoreStatement :: { [AnyStatement] }
    : Statement
    { [$1] }
    | OneOrMoreStatement ';' Statement
    { $1 ++ [$3] }

Statement :: { AnyStatement }
    : Explain
    { Statement $1 }
    | ExplainQueryPlan
    { Statement $1 }
    | AlterTable
    { Statement $1 }
    | Analyze
    { Statement $1 }
    | Attach
    { Statement $1 }
    | Begin
    { Statement $1 }
    | Commit
    { Statement $1 }
    | CreateIndex
    { Statement $1 }
    | CreateTable
    { Statement $1 }
    | CreateTrigger
    { Statement $1 }
    | CreateView
    { Statement $1 }
--     | CreateVirtualTable
--     { Statement $1 }
-- TODO don't forget to uncomment this
    | Delete
    { Statement $1 }
    | DeleteLimited
    { Statement $1 }
    | Detach
    { Statement $1 }
    | DropIndex
    { Statement $1 }
    | DropTable
    { Statement $1 }
    | DropTrigger
    { Statement $1 }
    | DropView
    { Statement $1 }
    | Insert
    { Statement $1 }
    | Pragma
    { Statement $1 }
    | Reindex
    { Statement $1 }
    | Release
    { Statement $1 }
    | Rollback
    { Statement $1 }
    | Savepoint
    { Statement $1 }
    | Select
    { Statement $1 }
    | Update
    { Statement $1 }
    | UpdateLimited
    { Statement $1 }
    | Vacuum
    { Statement $1 }

ExplainableStatement :: { ExplainableStatement }
    : AlterTable
    { ExplainableStatement $1 }
    | Analyze
    { ExplainableStatement $1 }
    | Attach
    { ExplainableStatement $1 }
    | Begin
    { ExplainableStatement $1 }
    | Commit
    { ExplainableStatement $1 }
    | CreateIndex
    { ExplainableStatement $1 }
    | CreateTable
    { ExplainableStatement $1 }
    | CreateTrigger
    { ExplainableStatement $1 }
--     | CreateVirtualTable
--     { ExplainableStatement $1 }
-- TODO don't forget to uncomment this
    | Delete
    { ExplainableStatement $1 }
    | DeleteLimited
    { ExplainableStatement $1 }
    | Detach
    { ExplainableStatement $1 }
    | DropIndex
    { ExplainableStatement $1 }
    | DropTable
    { ExplainableStatement $1 }
    | DropTrigger
    { ExplainableStatement $1 }
    | DropView
    { ExplainableStatement $1 }
    | Insert
    { ExplainableStatement $1 }
    | Pragma
    { ExplainableStatement $1 }
    | Reindex
    { ExplainableStatement $1 }
    | Release
    { ExplainableStatement $1 }
    | Rollback
    { ExplainableStatement $1 }
    | Savepoint
    { ExplainableStatement $1 }
    | Select
    { ExplainableStatement $1 }
    | Update
    { ExplainableStatement $1 }
    | UpdateLimited
    { ExplainableStatement $1 }
    | Vacuum
    { ExplainableStatement $1 }

Explain :: { Statement L1 NT NS }
    : explain ExplainableStatement
    { Explain $2 }

ExplainQueryPlan :: { Statement L1 NT NS }
    : explain query plan ExplainableStatement
    { ExplainQueryPlan $4 }

AlterTable :: { Statement L0 NT NS }
    : alter table SinglyQualifiedIdentifier AlterTableBody
    { AlterTable $3 $4 }

Analyze :: { Statement L0 NT NS }
    : analyze SinglyQualifiedIdentifier
    { Analyze $2 }

Attach :: { Statement L0 NT NS }
    : attach string as UnqualifiedIdentifier
    { Attach False $2 $4 }
    | attach database string as UnqualifiedIdentifier
    { Attach True $3 $5 }

Begin :: { Statement L0 NT NS }
    : begin MaybeTransactionType
    { Begin $2 False }
    | begin MaybeTransactionType transaction
    { Begin $2 True }

Commit :: { Statement L0 NT NS }
    : commit
    { Commit False False }
    | commit transaction
    { Commit False True }
    | end
    { Commit True False }
    | end transaction
    { Commit True True}

CreateIndex :: { Statement L0 NT NS }
    : create MaybeUnique index MaybeIfNotExists SinglyQualifiedIdentifier on
      UnqualifiedIdentifier '(' OneOrMoreIndexedColumn ')'
    { CreateIndex $2 $4 $5 $7 (fromJust $ mkOneOrMore $9) }

CreateTable :: { Statement L0 NT NS }
    : create Permanence table MaybeIfNotExists SinglyQualifiedIdentifier
      EitherColumnsAndConstraintsSelect
    { CreateTable $2 $4 $5 $6 }

CreateTrigger :: { Statement L0 NT NS }
    : create Permanence trigger MaybeIfNotExists SinglyQualifiedIdentifier
      TriggerTime TriggerCondition UnqualifiedIdentifier MaybeForEachRow
      MaybeWhenClause begin OneOrMoreTriggerStatement ';' end
    { CreateTrigger $2 $4 $5 $6 $7 $8 $9 $10 (fromJust $ mkOneOrMore $12) }

CreateView :: { Statement L0 NT NS }
    : create Permanence view MaybeIfNotExists SinglyQualifiedIdentifier as Select
    { CreateView $2 $4 $5 $7 }

-- CreateVirtualTable :: { Statement L0 NT NS }
--     :
--     { }
-- TODO definition (requires monadic parser)

Delete :: { Statement L0 T NS }
    : delete from QualifiedTableName MaybeWhereClause
    { Delete $3 $4 }

DeleteLimited :: { Statement L0 NT NS }
    : delete from QualifiedTableName MaybeWhereClause MaybeOrderClause LimitClause
    { DeleteLimited $3 $4 $5 $6 }

DeleteOrDeleteLimited :: { AnyStatement }
    : Delete
    { Statement $1 }
    | DeleteLimited
    { Statement $1 }

Detach :: { Statement L0 NT NS }
    : detach UnqualifiedIdentifier
    { Detach False $2 }
    | detach database UnqualifiedIdentifier
    { Detach True $3 }

DropIndex :: { Statement L0 NT NS }
    : drop index MaybeIfExists SinglyQualifiedIdentifier
    { DropIndex $3 $4 }

DropTable :: { Statement L0 NT NS }
    : drop table MaybeIfExists SinglyQualifiedIdentifier
    { DropTable $3 $4 }

DropTrigger :: { Statement L0 NT NS }
    : drop trigger MaybeIfExists SinglyQualifiedIdentifier
    { DropTrigger $3 $4 }

DropView :: { Statement L0 NT NS }
    : drop view MaybeIfExists SinglyQualifiedIdentifier
    { DropView $3 $4 }

Insert :: { Statement L0 T NS }
    : InsertHead into SinglyQualifiedIdentifier InsertBody
    { Insert $1 $3 $4 }

Pragma :: { Statement L0 NT NS }
    : pragma SinglyQualifiedIdentifier PragmaBody
    { Pragma $2 $3 }

Reindex :: { Statement L0 NT NS }
    : reindex SinglyQualifiedIdentifier
    { Reindex $2 }

Release :: { Statement L0 NT NS }
    : release UnqualifiedIdentifier
    { Release False $2 }
    | release savepoint UnqualifiedIdentifier
    { Release True $3 }

Rollback :: { Statement L0 NT NS }
    : rollback
    { Rollback False Nothing }
    | rollback transaction
    { Rollback True Nothing }
    | rollback to UnqualifiedIdentifier
    { Rollback False (Just (False, $3)) }
    | rollback to savepoint UnqualifiedIdentifier
    { Rollback False (Just (True, $4)) }
    | rollback transaction to UnqualifiedIdentifier
    { Rollback True (Just (False, $4)) }
    | rollback transaction to savepoint UnqualifiedIdentifier
    { Rollback True (Just (True, $5)) }

Savepoint :: { Statement L0 NT NS }
    : savepoint UnqualifiedIdentifier
    { Savepoint $2 }

Select :: { Statement L0 T S }
    : SelectCore SelectCoreList MaybeOrderClause MaybeLimitClause
    { Select $1 $2 $3 $4 }

Update :: { Statement L0 T NS }
    : UpdateHead QualifiedTableName set OneOrMoreSetPair MaybeWhereClause
    { Update $1 $2 (fromJust $ mkOneOrMore $4) $5 }

UpdateLimited :: { Statement L0 NT NS }
    : UpdateHead QualifiedTableName set OneOrMoreSetPair MaybeWhereClause
      MaybeOrderClause LimitClause
    { UpdateLimited $1 $2 (fromJust $ mkOneOrMore $4) $5 $6 $7 }

UpdateOrUpdateLimited :: { AnyStatement }
    : Update
    { Statement $1 }
    | UpdateLimited
    { Statement $1 }

Vacuum :: { Statement L0 NT NS }
    : vacuum
    { Vacuum }

UnqualifiedIdentifier :: { UnqualifiedIdentifier }
    : identifier
    { UnqualifiedIdentifier $1 }

OneOrMoreUnqualifiedIdentifier :: { [UnqualifiedIdentifier] }
    : UnqualifiedIdentifier
    { [$1] }
    | OneOrMoreUnqualifiedIdentifier ',' UnqualifiedIdentifier
    { $1 ++ [$3] }

SinglyQualifiedIdentifier :: { SinglyQualifiedIdentifier }
    : UnqualifiedIdentifier %prec LOOSER_THAN_DOT
    { let UnqualifiedIdentifier properName = $1
      in SinglyQualifiedIdentifier Nothing properName }
    | UnqualifiedIdentifier '.' UnqualifiedIdentifier
    { let { UnqualifiedIdentifier parentName = $1;
            UnqualifiedIdentifier properName = $3 }
      in SinglyQualifiedIdentifier (Just parentName) properName }

DoublyQualifiedIdentifier :: { DoublyQualifiedIdentifier }
    : SinglyQualifiedIdentifier
    { case $1 of
        SinglyQualifiedIdentifier Nothing properName
          -> DoublyQualifiedIdentifier Nothing properName
        SinglyQualifiedIdentifier (Just parentName) properName
          -> DoublyQualifiedIdentifier (Just (parentName, Nothing)) properName }
    | SinglyQualifiedIdentifier '.' identifier
    { let properName = $3
      in case $1 of
           SinglyQualifiedIdentifier Nothing parentName
             -> DoublyQualifiedIdentifier (Just (parentName, Nothing))
                                          properName
           SinglyQualifiedIdentifier (Just grandparentName) parentName
             -> DoublyQualifiedIdentifier (Just (parentName, Just grandparentName))
                                          properName }

{

readType :: String -> Type
readType input = parseType $ lexer input


readLikeType :: String -> LikeType
readLikeType input = parseLikeType $ lexer input


readExpression :: String -> Expression
readExpression input = parseExpression $ lexer input


readMaybeUnique :: String -> MaybeUnique
readMaybeUnique input = parseMaybeUnique $ lexer input


readMaybeIfNotExists :: String -> MaybeIfNotExists
readMaybeIfNotExists input = parseMaybeIfNotExists $ lexer input


readMaybeIfExists :: String -> MaybeIfExists
readMaybeIfExists input = parseMaybeIfExists $ lexer input


readMaybeForEachRow :: String -> MaybeForEachRow
readMaybeForEachRow input = parseMaybeForEachRow $ lexer input


readPermanence :: String -> Permanence
readPermanence input = parsePermanence $ lexer input


readMaybeCollation :: String -> MaybeCollation
readMaybeCollation input = parseMaybeCollation $ lexer input


readMaybeAscDesc :: String -> MaybeAscDesc
readMaybeAscDesc input = parseMaybeAscDesc $ lexer input


readMaybeAutoincrement :: String -> MaybeAutoincrement
readMaybeAutoincrement input = parseMaybeAutoincrement $ lexer input


readMaybeSign :: String -> MaybeSign
readMaybeSign input = parseMaybeSign $ lexer input


readAlterTableBody :: String -> AlterTableBody
readAlterTableBody input = parseAlterTableBody $ lexer input


readColumnDefinition :: String -> ColumnDefinition
readColumnDefinition input = parseColumnDefinition $ lexer input


readDefaultValue :: String -> DefaultValue
readDefaultValue input = parseDefaultValue $ lexer input


readIndexedColumn :: String -> IndexedColumn
readIndexedColumn input = parseIndexedColumn $ lexer input


readColumnConstraint :: String -> ColumnConstraint
readColumnConstraint input = parseColumnConstraint $ lexer input


readTableConstraint :: String -> TableConstraint
readTableConstraint input = parseTableConstraint $ lexer input


readTriggerTime :: String -> TriggerTime
readTriggerTime input = parseTriggerTime $ lexer input


readTriggerCondition :: String -> TriggerCondition
readTriggerCondition input = parseTriggerCondition $ lexer input


{- TODO remember to uncomment this
readModuleArgument :: String -> ModuleArgument
readModuleArgument input = parseModuleArgument $ lexer input
-}


readTriggerStatement :: String -> TriggerStatement
readTriggerStatement input = parseTriggerStatement $ lexer input


readQualifiedTableName :: String -> QualifiedTableName
readQualifiedTableName input = parseQualifiedTableName $ lexer input


readOrderingTerm :: String -> OrderingTerm
readOrderingTerm input = parseOrderingTerm $ lexer input


readPragmaBody :: String -> PragmaBody
readPragmaBody input = parsePragmaBody $ lexer input


readPragmaValue :: String -> PragmaValue
readPragmaValue input = parsePragmaValue $ lexer input


readEitherColumnsAndConstraintsSelect :: String -> EitherColumnsAndConstraintsSelect
readEitherColumnsAndConstraintsSelect input = parseEitherColumnsAndConstraintsSelect $ lexer input


readInsertHead :: String -> InsertHead
readInsertHead input = parseInsertHead $ lexer input


readInsertBody :: String -> InsertBody
readInsertBody input = parseInsertBody $ lexer input


readUpdateHead :: String -> UpdateHead
readUpdateHead input = parseUpdateHead $ lexer input


readDistinctness :: String -> Distinctness
readDistinctness input = parseDistinctness $ lexer input


readMaybeHaving :: String -> MaybeHaving
readMaybeHaving input = parseMaybeHaving $ lexer input


readMaybeAs :: String -> MaybeAs
readMaybeAs input = parseMaybeAs $ lexer input


readCompoundOperator :: String -> CompoundOperator
readCompoundOperator input = parseCompoundOperator $ lexer input


readSelectCore :: String -> SelectCore
readSelectCore input = parseSelectCore $ lexer input


readResultColumn :: String -> ResultColumn
readResultColumn input = parseResultColumn $ lexer input


readJoinSource :: String -> JoinSource
readJoinSource input = parseJoinSource $ lexer input


readSingleSource :: String -> SingleSource
readSingleSource input = parseSingleSource $ lexer input


readJoinOperation :: String -> JoinOperation
readJoinOperation input = parseJoinOperation $ lexer input


readJoinConstraint :: String -> JoinConstraint
readJoinConstraint input = parseJoinConstraint $ lexer input


readMaybeIndexedBy :: String -> MaybeIndexedBy
readMaybeIndexedBy input = parseMaybeIndexedBy $ lexer input


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


readForeignKeyClauseActionOrMatchPart :: String -> ForeignKeyClauseActionOrMatchPart
readForeignKeyClauseActionOrMatchPart input = parseForeignKeyClauseActionOrMatchPart $ lexer input


readForeignKeyClauseActionPart :: String -> ForeignKeyClauseActionPart
readForeignKeyClauseActionPart input = parseForeignKeyClauseActionPart $ lexer input


readForeignKeyClauseDeferrablePart :: String -> ForeignKeyClauseDeferrablePart
readForeignKeyClauseDeferrablePart input = parseForeignKeyClauseDeferrablePart $ lexer input


readMaybeInitialDeferralStatus :: String -> MaybeInitialDeferralStatus
readMaybeInitialDeferralStatus input = parseMaybeInitialDeferralStatus $ lexer input


readMaybeTransactionType :: String -> MaybeTransactionType
readMaybeTransactionType input = parseMaybeTransactionType $ lexer input


readStatementList :: String -> StatementList
readStatementList input = parseStatementList $ lexer input


readAnyStatement :: String -> AnyStatement
readAnyStatement input = parseAnyStatement $ lexer input


readExplainableStatement :: String -> ExplainableStatement
readExplainableStatement input = parseExplainableStatement $ lexer input


readExplain :: String -> Statement L1 NT NS
readExplain input = parseExplain $ lexer input


readExplainQueryPlan :: String -> Statement L1 NT NS
readExplainQueryPlan input = parseExplainQueryPlan $ lexer input


readAlterTable :: String -> Statement L0 NT NS
readAlterTable input = parseAlterTable $ lexer input


readAnalyze :: String -> Statement L0 NT NS
readAnalyze input = parseAnalyze $ lexer input


readAttach :: String -> Statement L0 NT NS
readAttach input = parseAttach $ lexer input


readBegin :: String -> Statement L0 NT NS
readBegin input = parseBegin $ lexer input


readCommit :: String -> Statement L0 NT NS
readCommit input = parseCommit $ lexer input


readCreateIndex :: String -> Statement L0 NT NS
readCreateIndex input = parseCreateIndex $ lexer input


readCreateTable :: String -> Statement L0 NT NS
readCreateTable input = parseCreateTable $ lexer input


readCreateTrigger :: String -> Statement L0 NT NS
readCreateTrigger input = parseCreateTrigger $ lexer input


readCreateView :: String -> Statement L0 NT NS
readCreateView input = parseCreateView $ lexer input


{- TODO remember to uncomment this
readCreateVirtualTable :: String -> Statement L0 NT NS
readCreateVirtualTable input = parseCreateVirtualTable $ lexer input
-}


readDelete :: String -> Statement L0 T NS
readDelete input = parseDelete $ lexer input


readDeleteLimited :: String -> Statement L0 NT NS
readDeleteLimited input = parseDeleteLimited $ lexer input


readDeleteOrDeleteLimited :: String -> AnyStatement
readDeleteOrDeleteLimited input = parseDeleteOrDeleteLimited $ lexer input


readDetach :: String -> Statement L0 NT NS
readDetach input = parseDetach $ lexer input


readDropIndex :: String -> Statement L0 NT NS
readDropIndex input = parseDropIndex $ lexer input


readDropTable :: String -> Statement L0 NT NS
readDropTable input = parseDropTable $ lexer input


readDropTrigger :: String -> Statement L0 NT NS
readDropTrigger input = parseDropTrigger $ lexer input


readDropView :: String -> Statement L0 NT NS
readDropView input = parseDropView $ lexer input


readInsert :: String -> Statement L0 T NS
readInsert input = parseInsert $ lexer input


readPragma :: String -> Statement L0 NT NS
readPragma input = parsePragma $ lexer input


readReindex :: String -> Statement L0 NT NS
readReindex input = parseReindex $ lexer input


readRelease :: String -> Statement L0 NT NS
readRelease input = parseRelease $ lexer input


readRollback :: String -> Statement L0 NT NS
readRollback input = parseRollback $ lexer input


readSavepoint :: String -> Statement L0 NT NS
readSavepoint input = parseSavepoint $ lexer input


readSelect :: String -> Statement L0 T S
readSelect input = parseSelect $ lexer input


readUpdate :: String -> Statement L0 T NS
readUpdate input = parseUpdate $ lexer input


readUpdateLimited :: String -> Statement L0 NT NS
readUpdateLimited input = parseUpdateLimited $ lexer input


readUpdateOrUpdateLimited :: String -> AnyStatement
readUpdateOrUpdateLimited input = parseUpdateOrUpdateLimited $ lexer input


readVacuum :: String -> Statement L0 NT NS
readVacuum input = parseVacuum $ lexer input


readUnqualifiedIdentifier :: String -> UnqualifiedIdentifier
readUnqualifiedIdentifier input = parseUnqualifiedIdentifier $ lexer input


readSinglyQualifiedIdentifier :: String -> SinglyQualifiedIdentifier
readSinglyQualifiedIdentifier input = parseSinglyQualifiedIdentifier $ lexer input


readDoublyQualifiedIdentifier :: String -> DoublyQualifiedIdentifier
readDoublyQualifiedIdentifier input = parseDoublyQualifiedIdentifier $ lexer input


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
