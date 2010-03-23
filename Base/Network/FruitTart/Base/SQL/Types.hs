{-# LANGUAGE GADTs, EmptyDataDecls, FlexibleInstances #-}
module Network.FruitTart.Base.SQL.Types (
                                         Type(..),
                                         LikeType(..),
                                         Expression(..),
                                         Statement(..),
                                         Token(..)
                                        )
    where

import qualified Data.ByteString as BS
import Data.Char
import Data.Int
import Data.List
import Numeric
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Typeable

import Network.FruitTart.Util


class ShowTokens a where
    showTokens :: a -> [Token]


data Type = Type String (Maybe (Either Double Int64, Maybe (Either Double Int64)))
instance ShowTokens Type where
    showTokens (Type name Nothing)
        = [Identifier name]
    showTokens (Type name (Just (maxLength, Nothing)))
        = [Identifier name,
           PunctuationLeftParenthesis]
          ++ showTokens maxLength
          ++ [PunctuationRightParenthesis]
    showTokens (Type name (Just (minLength, Just maxLength)))
        = [Identifier name,
           PunctuationLeftParenthesis]
          ++ showTokens minLength
          ++ [PunctuationComma]
          ++ showTokens maxLength
          ++ [PunctuationRightParenthesis]

instance ShowTokens (Either Double Int64) where
    showTokens (Left double)
        | double < 0 = [PunctuationMinus, LiteralFloat $ abs double]
        | otherwise = [LiteralFloat double]
    showTokens (Right integer)
        | integer < 0 = [PunctuationMinus, LiteralInteger $ abs integer]
        | otherwise = [LiteralInteger integer]

data LikeType = Like
              | NotLike
              | Glob
              | NotGlob
              | Regexp
              | NotRegexp
              | Match
              | NotMatch
instance ShowTokens LikeType where
    showTokens Like = [KeywordLike]
    showTokens NotLike = [KeywordNot, KeywordLike]
    showTokens Glob = [KeywordGlob]
    showTokens NotGlob = [KeywordNot, KeywordGlob]
    showTokens Regexp = [KeywordRegexp]
    showTokens NotRegexp = [KeywordNot, KeywordRegexp]
    showTokens Match = [KeywordMatch]
    showTokens NotMatch = [KeywordNot, KeywordMatch]

data Expression = ExpressionLiteralInteger Int64
                | ExpressionLiteralFloat Double
                | ExpressionLiteralString String
                | ExpressionLiteralBlob BS.ByteString
                | ExpressionLiteralNull
                | ExpressionLiteralCurrentTime
                | ExpressionLiteralCurrentDate
                | ExpressionLiteralCurrentTimestamp
                | ExpressionVariable
                | ExpressionVariableN Int64
                | ExpressionVariableNamed String
                | ExpressionIdentifier DoublyQualifiedIdentifier
                | ExpressionUnaryNegative Expression
                | ExpressionUnaryPositive Expression
                | ExpressionUnaryBitwiseNot Expression
                | ExpressionUnaryLogicalNot Expression
                | ExpressionBinaryConcatenate Expression Expression
                | ExpressionBinaryMultiply Expression Expression
                | ExpressionBinaryDivide Expression Expression
                | ExpressionBinaryModulus Expression Expression
                | ExpressionBinaryAdd Expression Expression
                | ExpressionBinarySubtract Expression Expression
                | ExpressionBinaryLeftShift Expression Expression
                | ExpressionBinaryRightShift Expression Expression
                | ExpressionBinaryBitwiseAnd Expression Expression
                | ExpressionBinaryBitwiseOr Expression Expression
                | ExpressionBinaryLess Expression Expression
                | ExpressionBinaryLessEquals Expression Expression
                | ExpressionBinaryGreater Expression Expression
                | ExpressionBinaryGreaterEquals Expression Expression
                | ExpressionBinaryEquals Expression Expression
                | ExpressionBinaryEqualsEquals Expression Expression
                | ExpressionBinaryNotEquals Expression Expression
                | ExpressionBinaryLessGreater Expression Expression
                | ExpressionBinaryLogicalAnd Expression Expression
                | ExpressionBinaryLogicalOr Expression Expression
                | ExpressionFunctionCall String [Expression]
                | ExpressionFunctionCallDistinct String [Expression]
                | ExpressionFunctionCallStar String
                | ExpressionCast Expression Type
                | ExpressionCollate Expression String
                | ExpressionLike Expression LikeType Expression (Maybe Expression)
                | ExpressionIsNull Expression
                | ExpressionNotnull Expression
                | ExpressionNotNull Expression
                | ExpressionIs Expression Expression
                | ExpressionIsNot Expression Expression
                | ExpressionBetween Expression Expression Expression
                | ExpressionNotBetween Expression Expression Expression
                | ExpressionInSelect Expression (Statement L0 T S)
                | ExpressionNotInSelect Expression (Statement L0 T S)
                | ExpressionInList Expression [Expression]
                | ExpressionNotInList Expression [Expression]
                | ExpressionInTable Expression SinglyQualifiedIdentifier
                | ExpressionNotInTable Expression SinglyQualifiedIdentifier
                | ExpressionSubquery (Statement L0 T S)
                | ExpressionExistsSubquery (Statement L0 T S)
                | ExpressionNotExistsSubquery (Statement L0 T S)
                | ExpressionCase (Maybe Expression)
                                 [(Expression, Expression)]
                                 (Maybe Expression)
                | ExpressionRaiseIgnore
                | ExpressionRaiseRollback String
                | ExpressionRaiseAbort String
                | ExpressionRaiseFail String

instance ShowTokens Expression where
    showTokens (ExpressionLiteralInteger integer)
        = [LiteralInteger $ abs integer]
    showTokens (ExpressionLiteralFloat double)
        = [LiteralFloat $ abs double]
    showTokens (ExpressionLiteralString string)
        = [LiteralString string]
    showTokens (ExpressionLiteralBlob bytestring)
        = [LiteralBlob bytestring]
    showTokens (ExpressionLiteralNull)
        = [KeywordNull]
    showTokens (ExpressionLiteralCurrentTime)
        = [KeywordCurrentTime]
    showTokens (ExpressionLiteralCurrentDate)
        = [KeywordCurrentDate]
    showTokens (ExpressionLiteralCurrentTimestamp)
        = [KeywordCurrentTimestamp]
    showTokens (ExpressionVariable)
        = [Variable]
    showTokens (ExpressionVariableN integer)
        = [VariableN integer]
    showTokens (ExpressionVariableNamed string)
        = [VariableNamed string]
    showTokens (ExpressionIdentifier doublyQualifiedIdentifier)
        = showTokens doublyQualifiedIdentifier
    showTokens (ExpressionUnaryNegative expression)
        = [PunctuationMinus] ++ showTokens expression
    showTokens (ExpressionUnaryPositive expression)
        = [PunctuationPlus] ++ showTokens expression
    showTokens (ExpressionUnaryBitwiseNot expression)
        = [PunctuationTilde] ++ showTokens expression
    showTokens (ExpressionUnaryLogicalNot expression)
        = [KeywordNot] ++ showTokens expression
    showTokens (ExpressionBinaryConcatenate a b)
        = showTokens a ++ [PunctuationBarBar] ++ showTokens b
    showTokens (ExpressionBinaryMultiply a b)
        = showTokens a ++ [PunctuationStar] ++ showTokens b
    showTokens (ExpressionBinaryDivide a b)
        = showTokens a ++ [PunctuationSlash] ++ showTokens b
    showTokens (ExpressionBinaryModulus a b)
        = showTokens a ++ [PunctuationPercent] ++ showTokens b
    showTokens (ExpressionBinaryAdd a b)
        = showTokens a ++ [PunctuationPlus] ++ showTokens b
    showTokens (ExpressionBinarySubtract a b)
        = showTokens a ++ [PunctuationMinus] ++ showTokens b
    showTokens (ExpressionBinaryLeftShift a b)
        = showTokens a ++ [PunctuationLessLess] ++ showTokens b
    showTokens (ExpressionBinaryRightShift a b)
        = showTokens a ++ [PunctuationGreaterGreater] ++ showTokens b
    showTokens (ExpressionBinaryBitwiseAnd a b)
        = showTokens a ++ [PunctuationAmpersand] ++ showTokens b
    showTokens (ExpressionBinaryBitwiseOr a b)
        = showTokens a ++ [PunctuationBar] ++ showTokens b
    showTokens (ExpressionBinaryLess a b)
        = showTokens a ++ [PunctuationLess] ++ showTokens b
    showTokens (ExpressionBinaryLessEquals a b)
        = showTokens a ++ [PunctuationLessEquals] ++ showTokens b
    showTokens (ExpressionBinaryGreater a b)
        = showTokens a ++ [PunctuationGreater] ++ showTokens b
    showTokens (ExpressionBinaryGreaterEquals a b)
        = showTokens a ++ [PunctuationGreaterEquals] ++ showTokens b
    showTokens (ExpressionBinaryEquals a b)
        = showTokens a ++ [PunctuationEquals] ++ showTokens b
    showTokens (ExpressionBinaryEqualsEquals a b)
        = showTokens a ++ [PunctuationEqualsEquals] ++ showTokens b
    showTokens (ExpressionBinaryNotEquals a b)
        = showTokens a ++ [PunctuationBangEquals] ++ showTokens b
    showTokens (ExpressionBinaryLessGreater a b)
        = showTokens a ++ [PunctuationLessGreater] ++ showTokens b
    showTokens (ExpressionBinaryLogicalAnd a b)
        = showTokens a ++ [KeywordAnd] ++ showTokens b
    showTokens (ExpressionBinaryLogicalOr a b)
        = showTokens a ++ [KeywordOr] ++ showTokens b
    showTokens (ExpressionFunctionCall name parameters)
        = [Identifier name,
           PunctuationLeftParenthesis]
          ++ intercalate [PunctuationComma] (map showTokens parameters)
          ++ [PunctuationRightParenthesis]
    showTokens (ExpressionFunctionCallDistinct name parameters)
        = [Identifier name,
           PunctuationLeftParenthesis,
           KeywordDistinct]
          ++ intercalate [PunctuationComma] (map showTokens parameters)
          ++ [PunctuationRightParenthesis]
    showTokens (ExpressionFunctionCallStar name)
        = [Identifier name,
           PunctuationLeftParenthesis,
           PunctuationStar,
           PunctuationRightParenthesis]
    showTokens (ExpressionCast expression typeDescriptor)
        = [KeywordCast,
           PunctuationLeftParenthesis]
          ++ showTokens expression
          ++ [KeywordAs]
          ++ showTokens typeDescriptor
          ++ [PunctuationRightParenthesis]
    showTokens (ExpressionCollate expression collation)
        = showTokens expression
          ++ [KeywordCollate, Identifier collation]
    showTokens (ExpressionLike a likeType b Nothing)
        = showTokens a
          ++ showTokens likeType
          ++ showTokens b
    showTokens (ExpressionLike a likeType b (Just escape))
        = showTokens a
          ++ showTokens likeType
          ++ showTokens b
          ++ [KeywordEscape]
          ++ showTokens escape
    showTokens (ExpressionIsNull expression)
        = showTokens expression
          ++ [KeywordIsnull]
    showTokens (ExpressionNotnull expression)
        = showTokens expression
          ++ [KeywordNotnull]
    showTokens (ExpressionNotNull expression)
        = showTokens expression
          ++ [KeywordNot, KeywordNull]
    showTokens (ExpressionIs a b)
        = showTokens a
          ++ [KeywordIs]
          ++ showTokens b
    showTokens (ExpressionIsNot a b)
        = showTokens a
          ++ [KeywordIs, KeywordNot]
          ++ showTokens b
    showTokens (ExpressionBetween a b c)
        = showTokens a
          ++ [KeywordBetween]
          ++ showTokens b
          ++ [KeywordAnd]
          ++ showTokens c
    showTokens (ExpressionNotBetween a b c)
        = showTokens a
          ++ [KeywordNot, KeywordBetween]
          ++ showTokens b
          ++ [KeywordAnd]
          ++ showTokens c
    showTokens (ExpressionInSelect expression statement)
        = showTokens expression
          ++ [KeywordIn, PunctuationLeftParenthesis]
          ++ showTokens statement
          ++ [PunctuationRightParenthesis]
    showTokens (ExpressionNotInSelect expression statement)
        = showTokens expression
          ++ [KeywordNot, KeywordIn, PunctuationLeftParenthesis]
          ++ showTokens statement
          ++ [PunctuationRightParenthesis]
    showTokens (ExpressionInList expression list)
        = showTokens expression
          ++ [KeywordIn, PunctuationLeftParenthesis]
          ++ intercalate [PunctuationComma] (map showTokens list)
          ++ [PunctuationRightParenthesis]
    showTokens (ExpressionNotInList expression list)
        = showTokens expression
          ++ [KeywordNot, KeywordIn, PunctuationLeftParenthesis]
          ++ intercalate [PunctuationComma] (map showTokens list)
          ++ [PunctuationRightParenthesis]
    showTokens (ExpressionInTable expression table)
        = showTokens expression
          ++ [KeywordIn]
          ++ showTokens table
    showTokens (ExpressionNotInTable expression table)
        = showTokens expression
          ++ [KeywordNot, KeywordIn]
          ++ showTokens table
    showTokens (ExpressionSubquery statement)
        = [PunctuationLeftParenthesis]
          ++ showTokens statement
          ++ [PunctuationRightParenthesis]
    showTokens (ExpressionExistsSubquery statement)
        = [KeywordExists, PunctuationLeftParenthesis]
          ++ showTokens statement
          ++ [PunctuationRightParenthesis]
    showTokens (ExpressionNotExistsSubquery statement)
        = [KeywordNot, KeywordExists, PunctuationLeftParenthesis]
          ++ showTokens statement
          ++ [PunctuationRightParenthesis]
    showTokens (ExpressionCase Nothing cases Nothing)
        = [KeywordCase]
          ++ (concat $ map (\(condition, result) -> [KeywordWhen]
                                                    ++ showTokens condition
                                                    ++ [KeywordThen]
                                                    ++ showTokens result)
                           cases)
          ++ [KeywordEnd]
    showTokens (ExpressionCase Nothing cases (Just defaultResult))
        = [KeywordCase]
          ++ (concat $ map (\(condition, result) -> [KeywordWhen]
                                                    ++ showTokens condition
                                                    ++ [KeywordThen]
                                                    ++ showTokens result)
                           cases)
          ++ [KeywordElse]
          ++ showTokens defaultResult
          ++ [KeywordEnd]
    showTokens (ExpressionCase (Just expression) cases Nothing)
        = [KeywordCase]
          ++ showTokens expression
          ++ (concat $ map (\(condition, result) -> [KeywordWhen]
                                                    ++ showTokens condition
                                                    ++ [KeywordThen]
                                                    ++ showTokens result)
                           cases)
          ++ [KeywordEnd]
    showTokens (ExpressionCase (Just expression) cases (Just defaultResult))
        = [KeywordCase]
          ++ showTokens expression
          ++ (concat $ map (\(condition, result) -> [KeywordWhen]
                                                    ++ showTokens condition
                                                    ++ [KeywordThen]
                                                    ++ showTokens result)
                           cases)
          ++ [KeywordElse]
          ++ showTokens defaultResult
          ++ [KeywordEnd]
    showTokens (ExpressionRaiseIgnore)
        = [KeywordRaise,
           PunctuationLeftParenthesis,
           KeywordIgnore,
           PunctuationRightParenthesis]
    showTokens (ExpressionRaiseRollback message)
        = [KeywordRaise,
           PunctuationLeftParenthesis,
           KeywordRollback,
           LiteralString message,
           PunctuationRightParenthesis]
    showTokens (ExpressionRaiseAbort message)
        = [KeywordRaise,
           PunctuationLeftParenthesis,
           KeywordAbort,
           LiteralString message,
           PunctuationRightParenthesis]
    showTokens (ExpressionRaiseFail message)
        = [KeywordRaise,
           PunctuationLeftParenthesis,
           KeywordFail,
           LiteralString message,
           PunctuationRightParenthesis]


-- | Used as a GADT parameter to Statement to indicate a type which can be EXPLAINed.
data L0
-- | Used as a GADT parameter to Statement to indicate a type which can not be EXPLAINed.
data L1

-- | Used as a GADT parameter to Statement to indicate a type which does not represent
--   a DELETE, INSERT, UPDATE, or SELECT statement.
data NT
-- | Used as a GADT parameter to Statement to indicate a type which represents
--   a DELETE, INSERT, UPDATE, or SELECT statement.
data T

-- | Used as a GADT parameter to Statement to indicate a type which does not represent
--   a SELECT statement.
data NS
-- | Used as a GADT parameter to Statement to indicate a type which represents a
--   SELECT statement.
data S

data MaybeIfNotExists = NoIfNotExists | IfNotExists
instance ShowTokens MaybeIfNotExists where
    showTokens NoIfNotExists = []
    showTokens IfNotExists = [KeywordIf, KeywordNot, KeywordExists]

data MaybeIfExists = NoIfExists | IfExists;
instance ShowTokens MaybeIfExists where
    showTokens NoIfExists = []
    showTokens IfExists = [KeywordIf, KeywordExists]

data MaybeForEachRow = NoForEachRow | ForEachRow;
instance ShowTokens MaybeForEachRow where
    showTokens NoForEachRow = []
    showTokens ForEachRow = [KeywordFor, KeywordEach, KeywordRow]

data Permanence = Permanent | Temp | Temporary;
instance ShowTokens Permanence where
    showTokens Permanent = []
    showTokens Temp = [KeywordTemp]
    showTokens Temporary = [KeywordTemporary]

data TriggerTime = Before | After | InsteadOf;
instance ShowTokens TriggerTime where
    showTokens Before = [KeywordBefore]
    showTokens After = [KeywordAfter]
    showTokens InsteadOf = [KeywordInstead, KeywordOf]
    
data TriggerCondition = DeleteOn | InsertOn | UpdateOn [UnqualifiedIdentifier];
instance ShowTokens TriggerCondition where
    showTokens DeleteOn = [KeywordDelete, KeywordOn]
    showTokens InsertOn = [KeywordInsert, KeywordOn]
    showTokens (UpdateOn []) = [KeywordUpdate, KeywordOn]
    showTokens (UpdateOn columnNames) = [KeywordUpdate, KeywordOf]
                                        ++ intercalate [PunctuationComma]
                                                       (map showTokens columnNames)
                                        ++ [KeywordOn]

data WhenClause = When Expression;
instance ShowTokens WhenClause where
    showTokens (When expression) = [KeywordWhen] ++ showTokens expression

data Statement level triggerable valueReturning where
    Explain :: Statement L0 a1 a2 -> Statement L1 NT NS
    ExplainQueryPlan :: Statement L0 a1 a2 -> Statement L1 NT NS
    AlterTable :: Statement L0 NT NS
    Analyze :: Statement L0 NT NS
    Attach :: Statement L0 NT NS
    Begin :: Statement L0 NT NS
    Commit :: Statement L0 NT NS
    CreateIndex :: Statement L0 NT NS
    CreateTable :: Statement L0 NT NS
    CreateTrigger
        :: Permanence
        -> MaybeIfNotExists
        -> SinglyQualifiedIdentifier
        -> TriggerTime
        -> TriggerCondition
        -> UnqualifiedIdentifier
        -> MaybeForEachRow
        -> (Maybe WhenClause)
        -> [Statement a1 T a2]
        -> Statement L0 NT NS
    CreateView :: Statement L0 NT NS
    CreateVirtualTable :: Statement L0 NT NS
    Delete :: Statement L0 T NS
    DeleteLimited :: Statement L0 NT NS
    Detach :: Statement L0 NT NS
    DropIndex :: Statement L0 NT NS
    DropTable :: Statement L0 NT NS
    DropTrigger :: Statement L0 NT NS
    DropView
        :: MaybeIfExists
        -> SinglyQualifiedIdentifier
        -> Statement L0 NT NS
    Insert :: Statement L0 T NS
    Pragma :: Statement L0 NT NS
    Reindex :: Statement L0 NT NS
    Release :: Statement L0 NT NS
    Rollback :: Statement L0 NT NS
    Savepoint :: Statement L0 NT NS
    Select :: Statement L0 T S
    Update :: Statement L0 T NS
    UpdateLimited :: Statement L0 NT NS
    Vacuum :: Statement L0 NT NS


instance ShowTokens (Statement a b c) where
    showTokens (Explain statement)
        = [KeywordExplain]
          ++ showTokens statement
    showTokens (ExplainQueryPlan statement)
        = [KeywordExplain, KeywordQuery, KeywordPlan]
          ++ showTokens statement
    showTokens AlterTable
        = [KeywordAlter, KeywordTable]
    showTokens Analyze
        = [KeywordAnalyze]
    showTokens Attach
        = [KeywordAttach]
    showTokens Begin
        = [KeywordBegin]
    showTokens Commit
        = [KeywordCommit]
    showTokens CreateIndex
        = [KeywordCreate, KeywordIndex]
    showTokens CreateTable
        = [KeywordCreate, KeywordTable]
    showTokens (CreateTrigger permanence maybeIfNotExists name time condition
                              tableName maybeForEachRow maybeWhenClause
                              statements)
        = [KeywordCreate]
          ++ showTokens permanence
          ++ [KeywordTrigger]
          ++ showTokens maybeIfNotExists
          ++ showTokens name
          ++ showTokens time
          ++ showTokens condition
          ++ showTokens tableName
          ++ showTokens maybeForEachRow
          ++ (case maybeWhenClause of
                Nothing -> []
                Just whenClause -> showTokens whenClause)
          ++ (intercalate [PunctuationSemicolon] $ map showTokens statements)
          ++ [KeywordEnd]
    showTokens CreateView
        = [KeywordCreate, KeywordView]
    showTokens CreateVirtualTable
        = [KeywordCreate, KeywordVirtual, KeywordTable]
    showTokens Delete
        = [KeywordDelete]
    showTokens DeleteLimited
        = [KeywordDelete, KeywordLimit]
    showTokens Detach
        = [KeywordDetach]
    showTokens DropIndex
        = [KeywordDrop, KeywordIndex]
    showTokens DropTable
        = [KeywordDrop, KeywordTable]
    showTokens DropTrigger
        = [KeywordDrop, KeywordTrigger]
    showTokens (DropView ifExists name)
        = [KeywordDrop, KeywordView]
          ++ showTokens ifExists
          ++ showTokens name
    showTokens Insert
        = [KeywordInsert]
    showTokens Pragma
        = [KeywordPragma]
    showTokens Reindex
        = [KeywordReindex]
    showTokens Release
        = [KeywordRelease]
    showTokens Rollback
        = [KeywordRollback]
    showTokens Savepoint
        = [KeywordSavepoint]
    showTokens Select
        = [KeywordSelect]
    showTokens Update
        = [KeywordUpdate]
    showTokens UpdateLimited
        = [KeywordUpdate, KeywordLimit]
    showTokens Vacuum
        = [KeywordVacuum]


data UnqualifiedIdentifier = UnqualifiedIdentifier String
instance ShowTokens UnqualifiedIdentifier where
    showTokens (UnqualifiedIdentifier properName)
        = [Identifier properName]


data SinglyQualifiedIdentifier
    = SinglyQualifiedIdentifier (Maybe String) String
instance ShowTokens SinglyQualifiedIdentifier where
    showTokens (SinglyQualifiedIdentifier Nothing properName)
        = [Identifier properName]
    showTokens (SinglyQualifiedIdentifier (Just databaseName) properName)
        = [Identifier databaseName, PunctuationDot, Identifier properName]


data DoublyQualifiedIdentifier
    = DoublyQualifiedIdentifier (Maybe (String, (Maybe String))) String
instance ShowTokens DoublyQualifiedIdentifier where
    showTokens (DoublyQualifiedIdentifier Nothing properName)
        = [Identifier properName]
    showTokens (DoublyQualifiedIdentifier (Just (tableName, Nothing)) properName)
        = [Identifier tableName,
           PunctuationDot,
           Identifier properName]
    showTokens (DoublyQualifiedIdentifier
                (Just (tableName, Just databaseName)) properName)
        = [Identifier databaseName,
           PunctuationDot,
           Identifier tableName,
           PunctuationDot,
           Identifier properName]


data Token = Identifier String
           | LiteralInteger Int64
           | LiteralFloat Double
           | LiteralString String
           | LiteralBlob BS.ByteString
           | Variable
           | VariableN Int64
           | VariableNamed String
           | PunctuationBarBar
           | PunctuationStar
           | PunctuationSlash
           | PunctuationPercent
           | PunctuationPlus
           | PunctuationMinus
           | PunctuationLessLess
           | PunctuationGreaterGreater
           | PunctuationAmpersand
           | PunctuationBar
           | PunctuationLess
           | PunctuationLessEquals
           | PunctuationGreater
           | PunctuationGreaterEquals
           | PunctuationEquals
           | PunctuationEqualsEquals
           | PunctuationBangEquals
           | PunctuationLessGreater
           | PunctuationTilde
           | PunctuationLeftParenthesis
           | PunctuationRightParenthesis
           | PunctuationComma
           | PunctuationDot
           | PunctuationSemicolon
           | KeywordAbort
           | KeywordAction
           | KeywordAdd
           | KeywordAfter
           | KeywordAll
           | KeywordAlter
           | KeywordAnalyze
           | KeywordAnd
           | KeywordAs
           | KeywordAsc
           | KeywordAttach
           | KeywordAutoincrement
           | KeywordBefore
           | KeywordBegin
           | KeywordBetween
           | KeywordBy
           | KeywordCascade
           | KeywordCase
           | KeywordCast
           | KeywordCheck
           | KeywordCollate
           | KeywordColumn
           | KeywordCommit
           | KeywordConflict
           | KeywordConstraint
           | KeywordCreate
           | KeywordCross
           | KeywordCurrentDate
           | KeywordCurrentTime
           | KeywordCurrentTimestamp
           | KeywordDatabase
           | KeywordDefault
           | KeywordDeferrable
           | KeywordDeferred
           | KeywordDelete
           | KeywordDesc
           | KeywordDetach
           | KeywordDistinct
           | KeywordDrop
           | KeywordEach
           | KeywordElse
           | KeywordEnd
           | KeywordEscape
           | KeywordExcept
           | KeywordExclusive
           | KeywordExists
           | KeywordExplain
           | KeywordFail
           | KeywordFor
           | KeywordForeign
           | KeywordFrom
           | KeywordFull
           | KeywordGlob
           | KeywordGroup
           | KeywordHaving
           | KeywordIf
           | KeywordIgnore
           | KeywordImmediate
           | KeywordIn
           | KeywordIndex
           | KeywordIndexed
           | KeywordInitially
           | KeywordInner
           | KeywordInsert
           | KeywordInstead
           | KeywordIntersect
           | KeywordInto
           | KeywordIs
           | KeywordIsnull
           | KeywordJoin
           | KeywordKey
           | KeywordLeft
           | KeywordLike
           | KeywordLimit
           | KeywordMatch
           | KeywordNatural
           | KeywordNo
           | KeywordNot
           | KeywordNotnull
           | KeywordNull
           | KeywordOf
           | KeywordOffset
           | KeywordOn
           | KeywordOr
           | KeywordOrder
           | KeywordOuter
           | KeywordPlan
           | KeywordPragma
           | KeywordPrimary
           | KeywordQuery
           | KeywordRaise
           | KeywordReferences
           | KeywordRegexp
           | KeywordReindex
           | KeywordRelease
           | KeywordRename
           | KeywordReplace
           | KeywordRestrict
           | KeywordRight
           | KeywordRollback
           | KeywordRow
           | KeywordSavepoint
           | KeywordSelect
           | KeywordSet
           | KeywordTable
           | KeywordTemp
           | KeywordTemporary
           | KeywordThen
           | KeywordTo
           | KeywordTransaction
           | KeywordTrigger
           | KeywordUnion
           | KeywordUnique
           | KeywordUpdate
           | KeywordUsing
           | KeywordVacuum
           | KeywordValues
           | KeywordView
           | KeywordVirtual
           | KeywordWhen
           | KeywordWhere


instance Show Token where
    show (Identifier identifier) =
        let validCharacter c = if isAscii c
                                 then (isAlphaNum c) || (elem c "_$")
                                 else True
            escapeCharacter '"' = "\"\""
            escapeCharacter c = [c]
        in if (all validCharacter identifier) && (not $ elem identifier keywordList)
             then identifier
             else "\"" ++ (concat $ map escapeCharacter identifier) ++ "\""
    show (LiteralInteger integer) = show integer
    show (LiteralFloat double) = show double
    show (LiteralString string) =
        let showChar char = case char of
                              '\'' -> "''"
                              _ -> [char]
            showString string = concat $ map showChar string
        in "'" ++ showString string ++ "'"
    show (LiteralBlob bytestring) =
        let showWord word = case showHex word "" of
                              [a] -> ['0', a]
                              a -> a
            showBytestring bytestring = concat $ map showWord $ BS.unpack bytestring
        in "x'" ++ showBytestring bytestring ++ "'"
    show Variable = "?"
    show (VariableN n) = "?" ++ (show n)
    show (VariableNamed name) = ":" ++ name
    show PunctuationBarBar = "||"
    show PunctuationStar = "*"
    show PunctuationSlash = "/"
    show PunctuationPercent = "%"
    show PunctuationPlus = "+" 
    show PunctuationMinus = "-"
    show PunctuationLessLess = "<<"
    show PunctuationGreaterGreater = ">>"
    show PunctuationAmpersand = "&"
    show PunctuationBar = "|"
    show PunctuationLess = "<"
    show PunctuationLessEquals = "<="
    show PunctuationGreater = ">"
    show PunctuationGreaterEquals = ">="
    show PunctuationEquals = "="
    show PunctuationEqualsEquals = "=="
    show PunctuationBangEquals = "!="
    show PunctuationLessGreater = "<>"
    show PunctuationTilde = "~"
    show PunctuationLeftParenthesis = "("
    show PunctuationRightParenthesis = ")"
    show PunctuationComma = ","
    show PunctuationDot = "."
    show PunctuationSemicolon = ";"
    show KeywordAbort = "ABORT"
    show KeywordAction = "ACTION"
    show KeywordAdd = "ADD"
    show KeywordAfter = "AFTER"
    show KeywordAll = "ALL"
    show KeywordAlter = "ALTER"
    show KeywordAnalyze = "ANALYZE"
    show KeywordAnd = "AND"
    show KeywordAs = "AS"
    show KeywordAsc = "ASC"
    show KeywordAttach = "ATTACH"
    show KeywordAutoincrement = "AUTOINCREMENT"
    show KeywordBefore = "BEFORE"
    show KeywordBegin = "BEGIN"
    show KeywordBetween = "BETWEEN"
    show KeywordBy = "BY"
    show KeywordCascade = "CASCADE"
    show KeywordCase = "CASE"
    show KeywordCast = "CAST"
    show KeywordCheck = "CHECK"
    show KeywordCollate = "COLLATE"
    show KeywordColumn = "COLUMN"
    show KeywordCommit = "COMMIT"
    show KeywordConflict = "CONFLICT"
    show KeywordConstraint = "CONSTRAINT"
    show KeywordCreate = "CREATE"
    show KeywordCross = "CROSS"
    show KeywordCurrentDate = "CURRENT_DATE"
    show KeywordCurrentTime = "CURRENT_TIME"
    show KeywordCurrentTimestamp = "CURRENT_TIMESTAMP"
    show KeywordDatabase = "DATABASE"
    show KeywordDefault = "DEFAULT"
    show KeywordDeferrable = "DEFERRABLE"
    show KeywordDeferred = "DEFERRED"
    show KeywordDelete = "DELETE"
    show KeywordDesc = "DESC"
    show KeywordDetach = "DETACH"
    show KeywordDistinct = "DISTINCT"
    show KeywordDrop = "DROP"
    show KeywordEach = "EACH"
    show KeywordElse = "ELSE"
    show KeywordEnd = "END"
    show KeywordEscape = "ESCAPE"
    show KeywordExcept = "EXCEPT"
    show KeywordExclusive = "EXCLUSIVE"
    show KeywordExists = "EXISTS"
    show KeywordExplain = "EXPLAIN"
    show KeywordFail = "FAIL"
    show KeywordFor = "FOR"
    show KeywordForeign = "FOREIGN"
    show KeywordFrom = "FROM"
    show KeywordFull = "FULL"
    show KeywordGlob = "GLOB"
    show KeywordGroup = "GROUP"
    show KeywordHaving = "HAVING"
    show KeywordIf = "IF"
    show KeywordIgnore = "IGNORE"
    show KeywordImmediate = "IMMEDIATE"
    show KeywordIn = "IN"
    show KeywordIndex = "INDEX"
    show KeywordIndexed = "INDEXED"
    show KeywordInitially = "INITIALLY"
    show KeywordInner = "INNER"
    show KeywordInsert = "INSERT"
    show KeywordInstead = "INSTEAD"
    show KeywordIntersect = "INTERSECT"
    show KeywordInto = "INTO"
    show KeywordIs = "IS"
    show KeywordIsnull = "ISNULL"
    show KeywordJoin = "JOIN"
    show KeywordKey = "KEY"
    show KeywordLeft = "LEFT"
    show KeywordLike = "LIKE"
    show KeywordLimit = "LIMIT"
    show KeywordMatch = "MATCH"
    show KeywordNatural = "NATURAL"
    show KeywordNo = "NO"
    show KeywordNot = "NOT"
    show KeywordNotnull = "NOTNULL"
    show KeywordNull = "NULL"
    show KeywordOf = "OF"
    show KeywordOffset = "OFFSET"
    show KeywordOn = "ON"
    show KeywordOr = "OR"
    show KeywordOrder = "ORDER"
    show KeywordOuter = "OUTER"
    show KeywordPlan = "PLAN"
    show KeywordPragma = "PRAGMA"
    show KeywordPrimary = "PRIMARY"
    show KeywordQuery = "QUERY"
    show KeywordRaise = "RAISE"
    show KeywordReferences = "REFERENCES"
    show KeywordRegexp = "REGEXP"
    show KeywordReindex = "REINDEX"
    show KeywordRelease = "RELEASE"
    show KeywordRename = "RENAME"
    show KeywordReplace = "REPLACE"
    show KeywordRestrict = "RESTRICT"
    show KeywordRight = "RIGHT"
    show KeywordRollback = "ROLLBACK"
    show KeywordRow = "ROW"
    show KeywordSavepoint = "SAVEPOINT"
    show KeywordSelect = "SELECT"
    show KeywordSet = "SET"
    show KeywordTable = "TABLE"
    show KeywordTemp = "TEMP"
    show KeywordTemporary = "TEMPORARY"
    show KeywordThen = "THEN"
    show KeywordTo = "TO"
    show KeywordTransaction = "TRANSACTION"
    show KeywordTrigger = "TRIGGER"
    show KeywordUnion = "UNION"
    show KeywordUnique = "UNIQUE"
    show KeywordUpdate = "UPDATE"
    show KeywordUsing = "USING"
    show KeywordVacuum = "VACUUM"
    show KeywordValues = "VALUES"
    show KeywordView = "VIEW"
    show KeywordVirtual = "VIRTUAL"
    show KeywordWhen = "WHEN"
    show KeywordWhere = "WHERE"


keywordList :: [String]
keywordList
    = ["ABORT", "ACTION", "ADD", "AFTER", "ALL", "ALTER", "ANALYZE", "AND", "AS",
       "ASC", "ATTACH", "AUTOINCREMENT", "BEFORE", "BEGIN", "BETWEEN", "BY", "CASCADE",
       "CASE", "CAST", "CHECK", "COLLATE", "COLUMN", "COMMIT", "CONFLICT", "CONSTRAINT",
       "CREATE", "CROSS", "CURRENT_DATE", "CURRENT_TIME", "CURRENT_TIMESTAMP",
       "DATABASE", "DEFAULT", "DEFERRABLE", "DEFERRED", "DELETE", "DESC", "DETACH",
       "DISTINCT", "DROP", "EACH", "ELSE", "END", "ESCAPE", "EXCEPT", "EXCLUSIVE",
       "EXISTS", "EXPLAIN", "FAIL", "FOR", "FOREIGN", "FROM", "FULL", "GLOB", "GROUP",
       "HAVING", "IF", "IGNORE", "IMMEDIATE", "IN", "INDEX", "INDEXED", "INITIALLY",
       "INNER", "INSERT", "INSTEAD", "INTERSECT", "INTO", "IS", "ISNULL", "JOIN",
       "KEY", "LEFT", "LIKE", "LIMIT", "MATCH", "NATURAL", "NO", "NOT", "NOTNULL",
       "NULL", "OF", "OFFSET", "ON", "OR", "ORDER", "OUTER", "PLAN", "PRAGMA",
       "PRIMARY", "QUERY", "RAISE", "REFERENCES", "REGEXP", "REINDEX", "RELEASE",
       "RENAME", "REPLACE", "RESTRICT", "RIGHT", "ROLLBACK", "ROW", "SAVEPOINT",
       "SELECT", "SET", "TABLE", "TEMP", "TEMPORARY", "THEN", "TO", "TRANSACTION",
       "TRIGGER", "UNION", "UNIQUE", "UPDATE", "USING", "VACUUM", "VALUES", "VIEW",
       "VIRTUAL", "WHEN", "WHERE"]
