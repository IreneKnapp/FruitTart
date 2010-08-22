{-# LANGUAGE GADTs #-}
module Network.FruitTart.Custard.Semantics (
                                            getTemplateWithParameters,
                                            applyFunctionGivenName,
                                            applyFunctionGivenContextAndValue,
                                            builtinBindings
                                           )
    where

import Control.Concurrent.MVar
import Control.Exception
import Control.Monad.State
import Data.List
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import Prelude hiding (catch)

import qualified Network.FastCGI as FCGI

import Network.FruitTart.Custard.Syntax
import Network.FruitTart.Custard.Functions.Util
import qualified Network.FruitTart.Custard.Functions.General as General
import qualified Network.FruitTart.Custard.Functions.HTTP as HTTP
import qualified Network.FruitTart.Custard.Functions.Forms as Forms
import qualified Network.FruitTart.Custard.Functions.Sessions as Sessions
import qualified Network.FruitTart.Custard.Functions.Captchas as Captchas
import qualified Network.FruitTart.Custard.Functions.Lists as Lists
import qualified Network.FruitTart.Custard.Functions.Strings as Strings
import Network.FruitTart.Types
import Network.FruitTart.Util


getTemplateWithParameters :: String
                          -> String
                          -> CustardContextType
                          -> Map String String
                          -> [CustardValue]
                          -> FruitTart String
getTemplateWithParameters moduleName
                          templateName
                          contextType
                          formInputMap
                          parameters = do
  let context = CustardContext {
                  custardContextType = contextType,
                  custardContextFormInputMap = formInputMap,
                  custardContextParameters = parameters,
                  custardContextLexicalBindings = Map.empty,
                  custardContextGlobalBindings = Map.empty
                }
  getTemplateWithContext moduleName templateName context


getTemplateWithContext :: String
                       -> String
                       -> CustardContext
                       -> FruitTart String
getTemplateWithContext moduleName templateName context = do
  items <- query ("SELECT template_items.kind, template_items.body "
                  ++ "FROM templates LEFT JOIN template_items "
                  ++ "ON templates.id = template_items.template "
                  ++ "WHERE templates.module = ? AND templates.name = ? "
                  ++ "ORDER BY template_items.item")
                 [SQLText moduleName, SQLText templateName]
  if items == []
    then error $ "Template " ++ moduleName ++ "." ++ templateName ++ " not found."
    else return ()
  foldM (\(context, accumulator) ([SQLText kind, SQLText body], index) -> do
          case kind of
            "content" -> return (context, accumulator ++ body)
            "expression" ->
                fCatch (do
                         FruitTartState { database = database } <- get
                         expression <- liftIO $ readExpression database
                                                               moduleName
                                                               body
                         (context, result)
                           <- evalExpression context expression
                         resultString <- valueToStringAllowingNull result
                         return (context, accumulator ++ resultString))
                       (\e -> error $ "While processing template "
                                    ++ moduleName ++ "."
                                    ++ templateName ++ ", item " ++ (show index)
                                    ++ ": " ++ (show (e :: SomeException)))
            _ -> error $ "Unknown template item type " ++ kind ++ ".")
        (context, "")
        (zip items [1..])
        >>= return . snd


eval :: String -> String -> FruitTart CustardValue
eval moduleName body = do
  let context = CustardContext {
                  custardContextType = TemplateContext,
                  custardContextFormInputMap = Map.empty,
                  custardContextParameters = [],
                  custardContextLexicalBindings = Map.empty,
                  custardContextGlobalBindings = Map.empty
                }
  FruitTartState { database = database } <- get
  expression <- liftIO $ readExpression database moduleName body
  (_, result) <- evalExpression context expression
  return result


valueToStringAllowingNull :: CustardValue -> FruitTart String
valueToStringAllowingNull (CustardString string)
  = return string
valueToStringAllowingNull CustardNull = return ""
valueToStringAllowingNull value
  = error $ "Value is not a String or Null."


evalExpression :: CustardContext
               -> CustardExpression
               -> FruitTart (CustardContext, CustardValue)
evalExpression context expression = do
    case expression of
      CustardLiteral value -> return (context, value)
      CustardExpressionList subexpressions -> do
        (context, values)
          <- foldM (\(context, values) subexpression -> do
                      (context, value) <- evalExpression context subexpression
                      return (context, values ++ [value]))
                   (context, [])
                   subexpressions
        return (context, CustardList values)
      CustardOperationConcatenate aExpression bExpression -> do
        (context, aValue) <- evalExpression context aExpression
        (context, bValue) <- evalExpression context bExpression
        case (aValue, bValue) of
          (CustardString aString, CustardString bString)
              -> return (context,
                         CustardString $ aString ++ bString)
          _ -> error "Cannot concatenate non-Strings."
      CustardOperationEquals aExpression bExpression -> do
        (context, aValue) <- evalExpression context aExpression
        (context, bValue) <- evalExpression context bExpression
        result <- custardEqual aValue bValue
        return (context, result)
      CustardOperationNotEquals aExpression bExpression -> do
        (context, aValue) <- evalExpression context aExpression
        (context, bValue) <- evalExpression context bExpression
        result <- custardEqual aValue bValue
        result <- custardNegate result
        return (context, result)
      CustardOperationNot expression -> do
        (context, value) <- evalExpression context expression
        result <- custardNegate value
        return (context, result)
      CustardOperationAnd aExpression bExpression -> do
        (context, aValue) <- evalExpression context aExpression
        (context, bValue) <- evalExpression context bExpression
        result <- custardAnd aValue bValue
        return (context, result)
      CustardOperationOr aExpression bExpression -> do
        (context, aValue) <- evalExpression context aExpression
        (context, bValue) <- evalExpression context bExpression
        result <- custardOr aValue bValue
        return (context, result)
      CustardOperationGreaterEquals aExpression bExpression -> do
        (context, aValue) <- evalExpression context aExpression
        (context, bValue) <- evalExpression context bExpression
        result <- custardCompare aValue bValue
        return (context, CustardBool $ case result of
                                         EQ -> True
                                         GT -> True
                                         LT -> False)
      CustardOperationGreater aExpression bExpression -> do
        (context, aValue) <- evalExpression context aExpression
        (context, bValue) <- evalExpression context bExpression
        result <- custardCompare aValue bValue
        return (context, CustardBool $ case result of
                                         EQ -> False
                                         GT -> True
                                         LT -> False)
      CustardOperationLessEquals aExpression bExpression -> do
        (context, aValue) <- evalExpression context aExpression
        (context, bValue) <- evalExpression context bExpression
        result <- custardCompare aValue bValue
        return (context, CustardBool $ case result of
                                         EQ -> True
                                         GT -> False
                                         LT -> True)
      CustardOperationLess aExpression bExpression -> do
        (context, aValue) <- evalExpression context aExpression
        (context, bValue) <- evalExpression context bExpression
        result <- custardCompare aValue bValue
        return (context, CustardBool $ case result of
                                         EQ -> False
                                         GT -> False
                                         LT -> True)
      CustardOperationAdd aExpression bExpression -> do
        (context, aValue) <- evalExpression context aExpression
        (context, bValue) <- evalExpression context bExpression
        result <- custardArithmetic aValue bValue (+)
        return (context, result)
      CustardOperationSubtract aExpression bExpression -> do
        (context, aValue) <- evalExpression context aExpression
        (context, bValue) <- evalExpression context bExpression
        result <- custardArithmetic aValue bValue (-)
        return (context, result)
      CustardOperationMultiply aExpression bExpression -> do
        (context, aValue) <- evalExpression context aExpression
        (context, bValue) <- evalExpression context bExpression
        result <- custardArithmetic aValue bValue (*)
        return (context, result)
      CustardOperationDivide aExpression bExpression -> do
        (context, aValue) <- evalExpression context aExpression
        (context, bValue) <- evalExpression context bExpression
        result <- custardArithmetic aValue bValue div
        return (context, result)
      CustardIfExpression subexpressions -> do
        if length subexpressions /= 3
          then error $ "Invalid number of parameters to if()."
          else return ()
        let condition = head subexpressions
            ifTrue = head $ drop 1 subexpressions
            ifFalse = head $ drop 2 subexpressions
        (context, result) <- evalExpression context condition
        result <- valueToBoolean result
        if result
          then evalExpression context ifTrue
          else evalExpression context ifFalse
      CustardCaseExpression subexpressions -> do
        let n = length subexpressions
        if not ((n > 1) && (odd n))
          then error $ "Invalid number of parameters to case()."
          else return ()
        (context, mainKey) <- evalExpression context $ head subexpressions
        let case' context items = do
              case items of
                [] -> error $ "No match in case() for " ++ (show mainKey) ++ "."
                (key:(value:rest)) -> do
                  case key of
                    CustardVariable ("Base", "otherwise") ->
                      evalExpression context value
                    _ -> do
                      (context, key) <- evalExpression context key
                      result <- custardEqual mainKey key
                      result <- valueToBoolean result
                      if result
                        then evalExpression context value
                        else case' context rest
        case' context $ tail subexpressions
      CustardCallExpression subexpressions -> do
        if length subexpressions < 1
           then error $ "Invalid number of parameters to call()."
           else return ()
        (moduleName, templateName)
            <- case head subexpressions of
                 CustardVariable result -> return result
                 _ -> error $ "First parameter to call() is not a symbol."
        (context, subparameters)
          <- foldM (\(context, values) subexpression -> do
                      (context, value) <- evalExpression context subexpression
                      return (context, values ++ [value]))
                   (context, [])
                   $ drop 1 subexpressions
        let subcontext = context {
                           custardContextParameters = subparameters,
                           custardContextType = TemplateContext
                         }
        result <- getTemplateWithContext moduleName templateName subcontext
        return (context, CustardString result)
      CustardIterateExpression subexpressions -> do
        if length subexpressions < 2
           then error $ "Invalid number of parameters to iterate()."
           else return ()
        (moduleName, templateName)
            <- case head subexpressions of
                 CustardVariable result -> return result
                 _ -> error $ "First parameter to iterate() is not a symbol."
        (context, rows) <- evalExpression context $ subexpressions !! 1
        rows <- valueToListOfMaps rows
        (context, subparameters)
          <- foldM (\(context, values) subexpression -> do
                      (context, value) <- evalExpression context subexpression
                      return (context, values ++ [value]))
                   (context, [])
                   $ drop 2 subexpressions
        results <- mapM (\row -> do
                           let CustardContext { custardContextLexicalBindings
                                                  = oldBindings } = context
                               newBindings = Map.union row oldBindings
                               subcontext
                                 = context {
                                     custardContextLexicalBindings
                                       = newBindings,
                                     custardContextType = TemplateContext
                                   }
                           getTemplateWithContext moduleName
                                                  templateName
                                                  subcontext)
                        rows
        return (context, CustardString $ concat results)
      CustardQueryExpression subexpressions -> do
        if length subexpressions < 1
           then error $ "Invalid number of parameters to query()."
           else return ()
        (context, parameters)
          <- foldM (\(context, values) subexpression -> do
                      (context, value) <- evalExpression context subexpression
                      return (context, values ++ [value]))
                   (context, [])
                   $ drop 1 subexpressions
        (moduleName, queryName)
            <- case head subexpressions of
                 CustardVariable result -> return result
                 _ -> error $ "First parameter to query() is not a symbol."
        rows <- namedQuery (moduleName, queryName) parameters
        let value = CustardList $ map CustardMap rows
        return (context, value)
      CustardBoundExpression subexpressions -> do
        if length subexpressions /= 1
          then error $ "Invalid number of parameters to bound()."
          else return ()
        name
            <- case head subexpressions of
                 CustardVariable result -> return result
                 _ -> error $ "Parameter to bound() is not a symbol."
        let CustardContext {
                      custardContextLexicalBindings = lexicalBindings,
                      custardContextGlobalBindings = globalBindings
                    } = context
        return (context,
                case Map.lookup name lexicalBindings of
                  Nothing -> case Map.lookup name globalBindings of
                               Nothing -> CustardBool False
                               Just _ -> CustardBool True
                  Just _ -> CustardBool True)
      CustardFunctionCall functionExpression actualParameterExpressions -> do
        (context, actualParameters)
          <- foldM (\(context, values) subexpression -> do
                      (context, value) <- evalExpression context subexpression
                      return (context, values ++ [value]))
                   (context, [])
                   actualParameterExpressions
        (context, function) <- evalExpression context functionExpression
        applyFunctionGivenContextAndValue context function actualParameters
      CustardLambdaExpression formalParameters body -> do
        let CustardContext { custardContextLexicalBindings = bindings }
              = context
        return (context, CustardLambda formalParameters bindings body)
      CustardVariable name -> do
        findSymbol context name
      CustardBindExpression subexpressions -> do
        if length subexpressions /= 2
          then error $ "Invalid number of parameters to bind()."
          else return ()
        name
            <- case head subexpressions of
                 CustardVariable result -> return result
                 _ -> error $ "Parameter to bind() is not a symbol."
        (context, value) <- evalExpression context $ subexpressions !! 1
        let CustardContext { custardContextGlobalBindings = oldBindings }
              = context
            newBindings = Map.union (Map.fromList [(name, value)])
                                    oldBindings
            context' = context { custardContextGlobalBindings = newBindings }
        return (context', CustardNull)
      CustardBindMapExpression subexpressions -> do
        if length subexpressions /= 1
          then error $ "Invalid number of parameters to bindMap()."
          else return ()
        (context, value) <- evalExpression context $ head subexpressions
        passedMap <- valueToMap value
        let CustardContext { custardContextGlobalBindings = oldBindings }
              = context
            newBindings = Map.union passedMap oldBindings
            context' = context { custardContextGlobalBindings = newBindings }
        return (context', CustardNull)
      CustardBindQuery1Expression subexpressions -> do
        if length subexpressions < 1
          then error $ "Invalid number of parameters to bindQuery1()."
          else return ()
        (moduleName, queryName)
            <- case head subexpressions of
                 CustardVariable result -> return result
                 _ -> error $ "Parameter to bindQuery1() is not a symbol."
        (context, inputs)
          <- foldM (\(context, values) subexpression -> do
                      (context, value) <- evalExpression context subexpression
                      return (context, values ++ [value]))
                   (context, [])
                   $ drop 1 subexpressions
        context <- bindQuery1 context (moduleName, queryName) inputs
        return (context, CustardNull)
      CustardBindQueryNExpression subexpressions -> do
        if length subexpressions < 1
          then error $ "Invalid number of parameters to bindQueryN()."
          else return ()
        (moduleName, queryName)
            <- case head subexpressions of
                 CustardVariable result -> return result
                 _ -> error $ "Parameter to bindQueryN() is not a symbol."
        (context, inputs)
          <- foldM (\(context, values) subexpression -> do
                      (context, value) <- evalExpression context subexpression
                      return (context, values ++ [value]))
                   (context, [])
                   $ drop 1 subexpressions
        context <- bindQueryN context (moduleName, queryName) inputs
        return (context, CustardNull)
      CustardLetExpression subexpressions -> do
        if length subexpressions /= 2
          then error $ "Invalid number of parameters to let()."
          else return ()
        name
            <- case head subexpressions of
                 CustardVariable result -> return result
                 _ -> error $ "Parameter to let() is not a symbol."
        (context, value) <- evalExpression context $ subexpressions !! 1
        let CustardContext { custardContextLexicalBindings = oldBindings }
              = context
            newBindings = Map.union (Map.fromList [(name, value)])
                                    oldBindings
            context' = context { custardContextLexicalBindings = newBindings }
        return (context', CustardNull)
      CustardLetMapExpression subexpressions -> do
        if length subexpressions /= 1
          then error $ "Invalid number of parameters to letMap()."
          else return ()
        (context, value) <- evalExpression context $ head subexpressions
        passedMap <- valueToMap value
        let CustardContext { custardContextLexicalBindings = oldBindings }
              = context
            newBindings = Map.union passedMap oldBindings
            context' = context { custardContextLexicalBindings = newBindings }
        return (context', CustardNull)
      CustardLetQuery1Expression subexpressions -> do
        if length subexpressions < 1
          then error $ "Invalid number of parameters to letQuery1()."
          else return ()
        (moduleName, queryName)
            <- case head subexpressions of
                 CustardVariable result -> return result
                 _ -> error $ "Parameter to letQuery1() is not a symbol."
        (context, inputs)
          <- foldM (\(context, values) subexpression -> do
                      (context, value) <- evalExpression context subexpression
                      return (context, values ++ [value]))
                   (context, [])
                   $ drop 1 subexpressions
        context <- letQuery1 context (moduleName, queryName) inputs
        return (context, CustardNull)
      CustardLetQueryNExpression subexpressions -> do
        if length subexpressions < 1
          then error $ "Invalid number of parameters to letQueryN()."
          else return ()
        (moduleName, queryName)
            <- case head subexpressions of
                 CustardVariable result -> return result
                 _ -> error $ "Parameter to letQueryN() is not a symbol."
        (context, inputs)
          <- foldM (\(context, values) subexpression -> do
                      (context, value) <- evalExpression context subexpression
                      return (context, values ++ [value]))
                   (context, [])
                   $ drop 1 subexpressions
        context <- letQueryN context (moduleName, queryName) inputs
        return (context, CustardNull)
      CustardSequence expressionA expressionB -> do
        (context, _) <- evalExpression context expressionA
        evalExpression context expressionB


custardEqual :: CustardValue
              -> CustardValue
              -> FruitTart CustardValue
custardEqual (CustardBool a) (CustardBool b)
    = return $ CustardBool $ a == b
custardEqual (CustardInteger a) (CustardInteger b)
    = return $ CustardBool $ a == b
custardEqual (CustardCharacter a) (CustardCharacter b)
    = return $ CustardBool $ a == b
custardEqual (CustardString a) (CustardString b)
    = return $ CustardBool $ a == b
custardEqual (CustardOrdering a) (CustardOrdering b)
    = return $ CustardBool $ a == b
custardEqual _ _
    = error $ "Values in comparison are not the same type or "
            ++ "are not Booleans, Integers, Characters, Strings, or Orderings."


custardNegate :: CustardValue
               -> FruitTart CustardValue
custardNegate (CustardBool value)
    = return $ CustardBool $ not value
custardNegate _ = error "Value in logical operation is not a Boolean."


custardAnd :: CustardValue
            -> CustardValue
            -> FruitTart CustardValue
custardAnd (CustardBool a) (CustardBool b)
    = return $ CustardBool $ a && b
custardAnd _ _ = error "Values in logical operation are not both Booleans."


custardOr :: CustardValue
           -> CustardValue
           -> FruitTart CustardValue
custardOr (CustardBool a) (CustardBool b)
    = return $ CustardBool $ a || b
custardOr _ _ = error "Values in logical operation are not both Booleans."


custardCompare :: CustardValue -> CustardValue -> FruitTart Ordering
custardCompare (CustardInteger a) (CustardInteger b) = return $ compare a b
custardCompare (CustardCharacter a) (CustardCharacter b) = return $ compare a b
custardCompare _ _ = error $  "Values in comparison are not the same type or "
                     ++ " are not Integers or Characters."


custardArithmetic :: CustardValue
                   -> CustardValue
                   -> (Int64 -> Int64 -> Int64)
                   -> FruitTart CustardValue
custardArithmetic (CustardInteger a) (CustardInteger b) operation
    = return $ CustardInteger $ operation a b
custardArithmetic _ _ _ = error "Values in arithmetic are not both Integers."


findSymbol :: CustardContext
           -> (String, String)
           -> FruitTart (CustardContext, CustardValue)
findSymbol context name@(packageName, properName) = do
  let CustardContext {
          custardContextLexicalBindings = lexicalBindings,
          custardContextGlobalBindings = globalBindings
        } = context
      bindings = Map.union lexicalBindings globalBindings
  case Map.lookup name bindings of
    Nothing -> case Map.lookup ("Base", properName) bindings of
                 Nothing -> do
                   maybeValue <- getTopLevelBinding name
                   case maybeValue of
                     Just value -> do
                       let CustardContext {
                                     custardContextGlobalBindings = oldBindings
                                   } = context
                           newBindings = Map.union
                                          (Map.fromList [(name, value)])
                                          oldBindings
                           context' = context {
                                        custardContextGlobalBindings = newBindings
                                      }
                       return (context', value)
                     Nothing -> error $ "Undefined variable "
                                        ++ packageName ++ "."
                                        ++ properName ++ "."
                 Just value -> return (context, value)
    Just value -> return (context, value)


applyFunctionGivenName :: CustardContextType
                       -> Map String String
                       -> String
                       -> String
                       -> [CustardValue]
                       -> FruitTart (CustardContext, CustardValue)
applyFunctionGivenName contextType
                       formInputMap
                       moduleName
                       functionName
                       parameters = do
  let context = CustardContext {
                  custardContextType = contextType,
                  custardContextFormInputMap = formInputMap,
                  custardContextParameters = [],
                  custardContextLexicalBindings = Map.empty,
                  custardContextGlobalBindings = Map.empty
                }
  (context, function) <- findSymbol context (moduleName, functionName)
  applyFunctionGivenContextAndValue context function parameters


applyFunctionGivenContextAndValue :: CustardContext
                                  -> CustardValue
                                  -> [CustardValue]
                                  -> FruitTart (CustardContext, CustardValue)
applyFunctionGivenContextAndValue context function actualParameters = do
  case function of
    CustardLambda formalParameters capturedBindings body -> do
      if length formalParameters /= length actualParameters
        then error $ "Expected " ++ (show $ length formalParameters)
                   ++ " parameters, but got " ++ (show $ length actualParameters)
                   ++ "."
        else return ()
      let newBindings = Map.fromList $ zip (map (\(CustardParameter key) -> key)
                                                formalParameters)
                                           actualParameters
          subbindings = Map.union newBindings capturedBindings
          context' = context { custardContextLexicalBindings = subbindings }
      (outputContext, result) <- evalExpression context' body
      let CustardContext { custardContextGlobalBindings = outputBindings }
            = outputContext
          outputContext'
            = context { custardContextGlobalBindings = outputBindings }
      return (outputContext, result)
    CustardNativeLambda body -> do
      result <- body context actualParameters
      return (context, result)
    _ -> do
      error $ "Call to something not a function."


getTopLevelBinding :: (String, String) -> FruitTart (Maybe CustardValue)
getTopLevelBinding variableName@(moduleName, functionName) = do
  case Map.lookup variableName builtinBindings of
    Just result -> return $ Just result
    Nothing -> do
      found <- query "SELECT id, body FROM functions WHERE module = ? AND name = ?"
                     [SQLText moduleName, SQLText functionName]
      case found of
        [[SQLInteger functionID, SQLText functionBody]] -> do
          FruitTartState { database = database } <- get
          compiledBody <- liftIO $ readExpression database moduleName functionBody
          parameterItems <- query ("SELECT name "
                                   ++ "FROM function_parameters "
                                   ++ "WHERE function = ? "
                                   ++ "ORDER BY item")
                                  [SQLInteger functionID]
          let parameters = map (\[SQLText parameterName]
                                 -> CustardParameter (moduleName, parameterName))
                               parameterItems
          return $ Just $ CustardLambda parameters Map.empty compiledBody
        _ -> return Nothing


bindQuery1 :: CustardContext
           -> (String, String)
           -> [CustardValue]
           -> FruitTart CustardContext
bindQuery1 context (moduleName, queryName) inputs = do
  rows <- namedQuery (moduleName, queryName) inputs
  case rows of
    [] -> error "No results from query."
    (row:_) -> do
      let CustardContext { custardContextGlobalBindings = oldBindings } = context
          newBindings = Map.union row oldBindings
          context' = context { custardContextGlobalBindings = newBindings }
      return context'


bindQueryN :: CustardContext
           -> (String, String)
           -> [CustardValue]
           -> FruitTart CustardContext
bindQueryN context name inputs = do
  rows <- namedQuery name inputs
  let value = CustardList $ map CustardMap rows
      CustardContext { custardContextGlobalBindings = oldBindings } = context
      newBindings = Map.union (Map.fromList [(name, value)]) oldBindings
      context' = context { custardContextGlobalBindings = newBindings }
  return context'


letQuery1 :: CustardContext
          -> (String, String)
          -> [CustardValue]
          -> FruitTart CustardContext
letQuery1 context (moduleName, queryName) inputs = do
  rows <- namedQuery (moduleName, queryName) inputs
  case rows of
    [] -> error "No results from query."
    (row:_) -> do
      let CustardContext { custardContextLexicalBindings = oldBindings } = context
          newBindings = Map.union row oldBindings
          context' = context { custardContextLexicalBindings = newBindings }
      return context'


letQueryN :: CustardContext
          -> (String, String)
          -> [CustardValue]
          -> FruitTart CustardContext
letQueryN context name inputs = do
  rows <- namedQuery name inputs
  let value = CustardList $ map CustardMap rows
      CustardContext { custardContextLexicalBindings = oldBindings } = context
      newBindings = Map.union (Map.fromList [(name, value)]) oldBindings
      context' = context { custardContextLexicalBindings = newBindings }
  return context'


namedQuery :: (String, String)
           -> [CustardValue]
           -> FruitTart [Map (String, String) CustardValue]
namedQuery (moduleName, queryName) inputs = do
  rows <- query ("SELECT id, body FROM queries "
                 ++ " WHERE module = ? AND name = ?")
                [SQLText moduleName, SQLText queryName]
  case rows of
    [[SQLInteger queryID, SQLText queryText]] -> do
      queryValues <- mapM convertQueryValue inputs
      valueNamesAndTypes <- getValueNamesAndTypes queryID
      rows <- query queryText queryValues
      return $ map (\row -> convertRowToBindings moduleName
                                                 valueNamesAndTypes
                                                 row)
                   rows
    _ -> error $ "Query " ++ moduleName ++ "." ++ queryName ++ " not found."


convertQueryValue :: CustardValue -> FruitTart SQLData
convertQueryValue (CustardBool True) = return $ SQLInteger 1
convertQueryValue (CustardBool False) = return $ SQLInteger 0
convertQueryValue (CustardMaybe Nothing) = return $ SQLNull
convertQueryValue (CustardMaybe (Just (CustardInteger integer)))
  = return $ SQLInteger integer
convertQueryValue (CustardMaybe (Just (CustardString string)))
  = return $ SQLText string
convertQueryValue (CustardInteger integer) = return $ SQLInteger integer
convertQueryValue (CustardString string) = return $ SQLText string
convertQueryValue _ = error "Invalid type for query parameter."


getValueNamesAndTypes :: Int64 -> FruitTart [(String, CustardValueType)]
getValueNamesAndTypes queryID = do
  rows <- query ("SELECT name, type FROM query_results "
                 ++ "WHERE query = ? ORDER BY item")
                [SQLInteger queryID]
  return $ map (\[SQLText name, SQLText typeName] ->
                 (name,
                  case typeName of
                    "boolean" -> CBool
                    "integer" -> CInt
                    "string" -> CString
                    "maybeInteger" -> CMaybeInt
                    "maybeString" -> CMaybeString
                    _ -> CInt))
               rows


convertRowToBindings :: String -> [(String, CustardValueType)] -> [SQLData]
                     -> Map (String, String) CustardValue
convertRowToBindings moduleName valueNamesAndTypes row
    = if length valueNamesAndTypes /= length row
        then error $ "Provided with " ++ (show $ length valueNamesAndTypes)
                   ++ " value names and types, but " ++ (show $ length row)
                   ++ " values."
        else
           Map.fromList
           $ map (\((columnName, valueType), value) ->
                      ((moduleName, columnName),
                       case valueType of
                         CBool -> case value of
                                    SQLInteger integer -> CustardBool
                                                          $ integer /= 0
                                    _ -> error "Value from query not an integer."
                         CInt -> case value of
                                   SQLInteger integer -> CustardInteger
                                                         $ integer
                                   _ -> error "Value from query not an integer."
                         CString -> case value of
                                      SQLText string -> CustardString string
                                      _ -> error "Value from query not a string."
                         CMaybeInt -> case value of
                                        SQLNull -> CustardMaybe Nothing
                                        SQLInteger integer -> CustardMaybe
                                                              $ Just
                                                              $ CustardInteger
                                                              $ integer
                                        _ -> error
                                             "Value from query not an integer or null."
                         CMaybeString -> case value of
                                        SQLNull -> CustardMaybe Nothing
                                        SQLText string -> CustardMaybe
                                                          $ Just
                                                          $ CustardString
                                                          $ string
                                        _ -> error
                                             "Value from query not a string or null."))
                 $ zip valueNamesAndTypes row


{-
bindQuery :: String -> [(String, TemplateValueType)]
          -> String -> [SQLData] -> FruitTart ()
bindQuery moduleName valueNamesAndTypes queryText queryValues = do
  [row] <- query queryText queryValues
  bindingsMVar <- getBindingsMVar
  oldBindings <- liftIO $ takeMVar bindingsMVar
  let newBindings = convertRowToBindings moduleName valueNamesAndTypes row
      bindings' = Map.union newBindings oldBindings
  liftIO $ putMVar bindingsMVar bindings'


bindQueryMultipleRows :: String -> String -> [(String, TemplateValueType)]
                      -> String -> [SQLData] -> FruitTart ()
bindQueryMultipleRows moduleName overallValueName valueNamesAndTypes
                      queryText queryValues = do
  rows <- query queryText queryValues
  bindingsMVar <- getBindingsMVar
  oldBindings <- liftIO $ takeMVar bindingsMVar
  let newBindings
          = Map.fromList [((moduleName, overallValueName),
                           CustardList
                           $ map (\row -> CustardMap
                                          $ convertRowToBindings moduleName
                                                                 valueNamesAndTypes
                                                                 row)
                                 rows)]
      bindings' = Map.union newBindings oldBindings
  liftIO $ putMVar bindingsMVar bindings'


bindNamedQuery :: String -> String -> [SQLData] -> FruitTart ()
bindNamedQuery moduleName queryName queryValues = do
  rows <- query ("SELECT id, body FROM queries "
                ++ "WHERE module = ? AND name = ?")
                [SQLText moduleName, SQLText queryName]
  case rows of
    [[SQLInteger queryID, SQLText queryText]] -> do
      valueNamesAndTypes <- getValueNamesAndTypes queryID
      bindQuery moduleName valueNamesAndTypes queryText queryValues
    _ -> error $ "Query " ++ moduleName ++ "." ++ queryName ++ " not found."


bindNamedQueryMultipleRows :: String -> String -> [SQLData] -> FruitTart ()
bindNamedQueryMultipleRows moduleName queryName queryValues = do
  rows <- query ("SELECT id, body FROM queries "
                ++ "WHERE module = ? AND name = ?")
                [SQLText moduleName, SQLText queryName]
  case rows of
    [[SQLInteger queryID, SQLText queryText]] -> do
      valueNamesAndTypes <- getValueNamesAndTypes queryID
      bindQueryMultipleRows moduleName queryName valueNamesAndTypes queryText
                            queryValues
    _ -> error $ "Query " ++ moduleName ++ "." ++ queryName ++ " not found."
 -}


builtinBindings :: Map (String, String) CustardValue
builtinBindings = Map.fromList
               [(("Base", "Null"),
                 CustardNull),
                (("Base", "True"),
                 CustardBool True),
                (("Base", "False"),
                 CustardBool False),
                (("Base", "Nothing"),
                 CustardMaybe Nothing),
                (("Base", "Just"),
                 CustardNativeLambda General.cfJust),
                (("Base", "LT"),
                 CustardOrdering LT),
                (("Base", "GT"),
                 CustardOrdering GT),
                (("Base", "EQ"),
                 CustardOrdering EQ),
                (("Base", "parameter"),
                 CustardNativeLambda General.cfParameter),
                (("Base", "isNothing"),
                 CustardNativeLambda General.cfIsNothing),
                (("Base", "isJust"),
                 CustardNativeLambda General.cfIsJust),
                (("Base", "fromJust"),
                 CustardNativeLambda General.cfFromJust),
                (("Base", "compareIntegers"),
                 CustardNativeLambda General.cfCompareIntegers),
                (("Base", "showInteger"),
                 CustardNativeLambda General.cfShowInteger),
                (("Base", "showBool"),
                 CustardNativeLambda General.cfShowBool),
                (("Base", "byteSizeToString"),
                 CustardNativeLambda General.cfByteSizeToString),
                (("Base", "timestampToString"),
                 CustardNativeLambda General.cfTimestampToString),
                (("Base", "escapeAttribute"),
                 CustardNativeLambda General.cfEscapeAttribute),
                (("Base", "escapeHTML"),
                 CustardNativeLambda General.cfEscapeHTML),
                (("Base", "newlinesToParagraphs"),
                 CustardNativeLambda General.cfNewlinesToParagraphs),
                
                -- HTTP
                (("Base", "log"),
                 CustardNativeLambda HTTP.cfLog),
                (("Base", "getRequestVariable"),
                 CustardNativeLambda HTTP.cfGetRequestVariable),
                (("Base", "getAllRequestVariables"),
                 CustardNativeLambda HTTP.cfGetAllRequestVariables),
                (("Base", "HttpAccept"),
                 CustardHTTPHeader FCGI.HttpAccept),
                (("Base", "HttpAcceptCharset"),
                 CustardHTTPHeader FCGI.HttpAcceptCharset),
                (("Base", "HttpAcceptEncoding"),
                 CustardHTTPHeader FCGI.HttpAcceptEncoding),
                (("Base", "HttpAcceptLanguage"),
                 CustardHTTPHeader FCGI.HttpAcceptLanguage),
                (("Base", "HttpAuthorization"),
                 CustardHTTPHeader FCGI.HttpAuthorization),
                (("Base", "HttpExpect"),
                 CustardHTTPHeader FCGI.HttpExpect),
                (("Base", "HttpFrom"),
                 CustardHTTPHeader FCGI.HttpFrom),
                (("Base", "HttpHost"),
                 CustardHTTPHeader FCGI.HttpHost),
                (("Base", "HttpIfMatch"),
                 CustardHTTPHeader FCGI.HttpIfMatch),
                (("Base", "HttpIfModifiedSince"),
                 CustardHTTPHeader FCGI.HttpIfModifiedSince),
                (("Base", "HttpIfNoneMatch"),
                 CustardHTTPHeader FCGI.HttpIfNoneMatch),
                (("Base", "HttpIfRange"),
                 CustardHTTPHeader FCGI.HttpIfRange),
                (("Base", "HttpIfUnmodifiedSince"),
                 CustardHTTPHeader FCGI.HttpIfUnmodifiedSince),
                (("Base", "HttpMaxForwards"),
                 CustardHTTPHeader FCGI.HttpMaxForwards),
                (("Base", "HttpProxyAuthorization"),
                 CustardHTTPHeader FCGI.HttpProxyAuthorization),
                (("Base", "HttpRange"),
                 CustardHTTPHeader FCGI.HttpRange),
                (("Base", "HttpReferer"),
                 CustardHTTPHeader FCGI.HttpReferer),
                (("Base", "HttpTE"),
                 CustardHTTPHeader FCGI.HttpTE),
                (("Base", "HttpUserAgent"),
                 CustardHTTPHeader FCGI.HttpUserAgent),
                (("Base", "HttpAcceptRanges"),
                 CustardHTTPHeader FCGI.HttpAcceptRanges),
                (("Base", "HttpAge"),
                 CustardHTTPHeader FCGI.HttpAge),
                (("Base", "HttpETag"),
                 CustardHTTPHeader FCGI.HttpETag),
                (("Base", "HttpLocation"),
                 CustardHTTPHeader FCGI.HttpLocation),
                (("Base", "HttpProxyAuthenticate"),
                 CustardHTTPHeader FCGI.HttpProxyAuthenticate),
                (("Base", "HttpRetryAfter"),
                 CustardHTTPHeader FCGI.HttpRetryAfter),
                (("Base", "HttpServer"),
                 CustardHTTPHeader FCGI.HttpServer),
                (("Base", "HttpVary"),
                 CustardHTTPHeader FCGI.HttpVary),
                (("Base", "HttpWWWAuthenticate"),
                 CustardHTTPHeader FCGI.HttpWWWAuthenticate),
                (("Base", "HttpAllow"),
                 CustardHTTPHeader FCGI.HttpAllow),
                (("Base", "HttpContentEncoding"),
                 CustardHTTPHeader FCGI.HttpContentEncoding),
                (("Base", "HttpContentLanguage"),
                 CustardHTTPHeader FCGI.HttpContentLanguage),
                (("Base", "HttpContentLength"),
                 CustardHTTPHeader FCGI.HttpContentLength),
                (("Base", "HttpContentLocation"),
                 CustardHTTPHeader FCGI.HttpContentLocation),
                (("Base", "HttpContentMD5"),
                 CustardHTTPHeader FCGI.HttpContentMD5),
                (("Base", "HttpContentRange"),
                 CustardHTTPHeader FCGI.HttpContentRange),
                (("Base", "HttpContentType"),
                 CustardHTTPHeader FCGI.HttpContentType),
                (("Base", "HttpExpires"),
                 CustardHTTPHeader FCGI.HttpExpires),
                (("Base", "HttpLastModified"),
                 CustardHTTPHeader FCGI.HttpLastModified),
                (("Base", "HttpConnection"),
                 CustardHTTPHeader FCGI.HttpConnection),
                (("Base", "HttpCookie"),
                 CustardHTTPHeader FCGI.HttpCookie),
                (("Base", "HttpSetCookie"),
                 CustardHTTPHeader FCGI.HttpSetCookie),
                (("Base", "getRequestHeader"),
                 CustardNativeLambda HTTP.cfGetRequestHeader),
                (("Base", "cookieName"),
                 CustardNativeLambda HTTP.cfCookieName),
                (("Base", "cookieValue"),
                 CustardNativeLambda HTTP.cfCookieValue),
                (("Base", "cookieVersion"),
                 CustardNativeLambda HTTP.cfCookieVersion),
                (("Base", "cookiePath"),
                 CustardNativeLambda HTTP.cfCookiePath),
                (("Base", "cookieDomain"),
                 CustardNativeLambda HTTP.cfCookieDomain),
                (("Base", "cookieMaxAge"),
                 CustardNativeLambda HTTP.cfCookieMaxAge),
                (("Base", "cookieSecure"),
                 CustardNativeLambda HTTP.cfCookieSecure),
                (("Base", "cookieComment"),
                 CustardNativeLambda HTTP.cfCookieComment),
                (("Base", "getCookie"),
                 CustardNativeLambda HTTP.cfGetCookie),
                (("Base", "getAllCookies"),
                 CustardNativeLambda HTTP.cfGetAllCookies),
                (("Base", "getCookieValue"),
                 CustardNativeLambda HTTP.cfGetCookieValue),
                (("Base", "getDocumentRoot"),
                 CustardNativeLambda HTTP.cfGetDocumentRoot),
                (("Base", "getGatewayInterface"),
                 CustardNativeLambda HTTP.cfGetGatewayInterface),
                (("Base", "getPathInfo"),
                 CustardNativeLambda HTTP.cfGetPathInfo),
                (("Base", "getPathTranslated"),
                 CustardNativeLambda HTTP.cfGetPathTranslated),
                (("Base", "getQueryString"),
                 CustardNativeLambda HTTP.cfGetQueryString),
                (("Base", "getRedirectStatus"),
                 CustardNativeLambda HTTP.cfGetRedirectStatus),
                (("Base", "getRedirectURI"),
                 CustardNativeLambda HTTP.cfGetRedirectURI),
                (("Base", "getRemoteAddress"),
                 CustardNativeLambda HTTP.cfGetRemoteAddress),
                (("Base", "getRemotePort"),
                 CustardNativeLambda HTTP.cfGetRemotePort),
                (("Base", "getRemoteHost"),
                 CustardNativeLambda HTTP.cfGetRemoteHost),
                (("Base", "getRemoteIdent"),
                 CustardNativeLambda HTTP.cfGetRemoteIdent),
                (("Base", "getRemoteUser"),
                 CustardNativeLambda HTTP.cfGetRemoteUser),
                (("Base", "getRequestMethod"),
                 CustardNativeLambda HTTP.cfGetRequestMethod),
                (("Base", "getRequestURI"),
                 CustardNativeLambda HTTP.cfGetRequestURI),
                (("Base", "getScriptFilename"),
                 CustardNativeLambda HTTP.cfGetScriptFilename),
                (("Base", "getScriptName"),
                 CustardNativeLambda HTTP.cfGetScriptName),
                (("Base", "getServerAddress"),
                 CustardNativeLambda HTTP.cfGetServerAddress),
                (("Base", "getServerName"),
                 CustardNativeLambda HTTP.cfGetServerName),
                (("Base", "getServerPort"),
                 CustardNativeLambda HTTP.cfGetServerPort),
                (("Base", "getServerProtocol"),
                 CustardNativeLambda HTTP.cfGetServerProtocol),
                (("Base", "getServerSoftware"),
                 CustardNativeLambda HTTP.cfGetServerSoftware),
                (("Base", "getAuthenticationType"),
                 CustardNativeLambda HTTP.cfGetAuthenticationType),
                (("Base", "getContentLength"),
                 CustardNativeLambda HTTP.cfGetContentLength),
                (("Base", "getContentType"),
                 CustardNativeLambda HTTP.cfGetContentType),
                (("Base", "setResponseStatus"),
                 CustardNativeLambda HTTP.cfSetResponseStatus),
                (("Base", "getResponseStatus"),
                 CustardNativeLambda HTTP.cfGetResponseStatus),
                (("Base", "setResponseHeader"),
                 CustardNativeLambda HTTP.cfSetResponseHeader),
                (("Base", "unsetResponseHeader"),
                 CustardNativeLambda HTTP.cfUnsetResponseHeader),
                (("Base", "getResponseHeader"),
                 CustardNativeLambda HTTP.cfGetResponseHeader),
                (("Base", "setCookie"),
                 CustardNativeLambda HTTP.cfSetCookie),
                (("Base", "unsetCookie"),
                 CustardNativeLambda HTTP.cfUnsetCookie),
                (("Base", "makeSimpleCookie"),
                 CustardNativeLambda HTTP.cfMakeSimpleCookie),
                (("Base", "makeCookie"),
                 CustardNativeLambda HTTP.cfMakeCookie),
                (("Base", "permanentRedirect"),
                 CustardNativeLambda HTTP.cfPermanentRedirect),
                (("Base", "seeOtherRedirect"),
                 CustardNativeLambda HTTP.cfSeeOtherRedirect),
                (("Base", "sendResponseHeaders"),
                 CustardNativeLambda HTTP.cfSendResponseHeaders),
                (("Base", "responseHeadersSent"),
                 CustardNativeLambda HTTP.cfResponseHeadersSent),
                (("Base", "put"),
                 CustardNativeLambda HTTP.cfPut),
                (("Base", "putString"),
                 CustardNativeLambda HTTP.cfPutString),
                (("Base", "closeOutput"),
                 CustardNativeLambda HTTP.cfCloseOutput),
                
                -- Forms
                (("Base", "formInput"),
                 CustardNativeLambda Forms.cfFormInput),
                
                -- Sessions
                (("Base", "getSessionID"),
                 CustardNativeLambda Sessions.cfGetSessionID),
                
                -- Captchas
                (("Base", "generateCaptcha"),
                 CustardNativeLambda Captchas.cfGenerateCaptcha),
                (("Base", "lookupCaptcha"),
                 CustardNativeLambda Captchas.cfLookupCaptcha),
                (("Base", "checkCaptcha"),
                 CustardNativeLambda Captchas.cfCheckCaptcha),
                (("Base", "expireOldCaptchas"),
                 CustardNativeLambda Captchas.cfExpireOldCaptchas),
                
                -- Everything below here is based on a corresponding function
                -- in Haskell, which may or may not have an identical name.
                
                -- Lists
                -- Lists - Basic functions
                (("Base", "head"),
                 CustardNativeLambda Lists.cfHead),
                (("Base", "last"),
                 CustardNativeLambda Lists.cfLast),
                (("Base", "tail"),
                 CustardNativeLambda Lists.cfTail),
                (("Base", "init"),
                 CustardNativeLambda Lists.cfInit),
                (("Base", "null"),
                 CustardNativeLambda Lists.cfNull),
                (("Base", "length"),
                 CustardNativeLambda Lists.cfLength),
                -- Lists - List transformations
                (("Base", "map"),
                 CustardNativeLambda Lists.cfMap),
                (("Base", "reverse"),
                 CustardNativeLambda Lists.cfReverse),
                (("Base", "intersperse"),
                 CustardNativeLambda Lists.cfIntersperse),
                (("Base", "intercalate"),
                 CustardNativeLambda Lists.cfIntercalate),
                (("Base", "transpose"),
                 CustardNativeLambda Lists.cfTranspose),
                (("Base", "subsequences"),
                 CustardNativeLambda Lists.cfSubsequences),
                (("Base", "permutations"),
                 CustardNativeLambda Lists.cfPermutations),
                -- Lists - Reducing lists (folds)
                (("Base", "foldl"),
                 CustardNativeLambda Lists.cfFoldl),
                (("Base", "foldl1"),
                 CustardNativeLambda Lists.cfFoldl1),
                (("Base", "foldr"),
                 CustardNativeLambda Lists.cfFoldr),
                (("Base", "foldr1"),
                 CustardNativeLambda Lists.cfFoldr1),
                -- Lists - Reducing lists (folds) - Special folds
                (("Base", "concat"),
                 CustardNativeLambda Lists.cfConcat),
                (("Base", "concatMap"),
                 CustardNativeLambda Lists.cfConcatMap),
                (("Base", "and"),
                 CustardNativeLambda Lists.cfAnd),
                (("Base", "or"),
                 CustardNativeLambda Lists.cfOr),
                (("Base", "any"),
                 CustardNativeLambda Lists.cfAny),
                (("Base", "all"),
                 CustardNativeLambda Lists.cfAll),
                (("Base", "sum"),
                 CustardNativeLambda Lists.cfSum),
                (("Base", "product"),
                 CustardNativeLambda Lists.cfProduct),
                (("Base", "maximum"),
                 CustardNativeLambda Lists.cfMaximum),
                (("Base", "minimum"),
                 CustardNativeLambda Lists.cfMinimum),
                -- Lists - Building lists
                -- Lists - Building lists - Scans
                (("Base", "scanl"),
                 CustardNativeLambda Lists.cfScanl),
                (("Base", "scanl1"),
                 CustardNativeLambda Lists.cfScanl1),
                (("Base", "scanr"),
                 CustardNativeLambda Lists.cfScanr),
                (("Base", "scanr1"),
                 CustardNativeLambda Lists.cfScanr1),
                -- Lists - Building lists - Accumulating maps
                (("Base", "mapAccumL"),
                 CustardNativeLambda Lists.cfMapAccumL),
                (("Base", "mapAccumR"),
                 CustardNativeLambda Lists.cfMapAccumR),
                -- Lists - Building lists - Replicate
                (("Base", "replicate"),
                 CustardNativeLambda Lists.cfReplicate),
                -- Lists - Building lists - Unfolding
                (("Base", "unfoldr"),
                 CustardNativeLambda Lists.cfUnfoldr),
                -- Lists - Sublists
                -- Lists - Sublists - Extracting sublists
                (("Base", "take"),
                 CustardNativeLambda Lists.cfTake),
                (("Base", "drop"),
                 CustardNativeLambda Lists.cfDrop),
                (("Base", "splitAt"),
                 CustardNativeLambda Lists.cfSplitAt),
                (("Base", "takeWhile"),
                 CustardNativeLambda Lists.cfTakeWhile),
                (("Base", "dropWhile"),
                 CustardNativeLambda Lists.cfDropWhile),
                (("Base", "span"),
                 CustardNativeLambda Lists.cfSpan),
                (("Base", "break"),
                 CustardNativeLambda Lists.cfBreak),
                (("Base", "stripPrefix"),
                 CustardNativeLambda Lists.cfStripPrefix),
                (("Base", "group"),
                 CustardNativeLambda Lists.cfGroup),
                (("Base", "inits"),
                 CustardNativeLambda Lists.cfInits),
                (("Base", "tails"),
                 CustardNativeLambda Lists.cfTails),
                -- Lists - Sublists - Predicates
                (("Base", "isPrefixOf"),
                 CustardNativeLambda Lists.cfIsPrefixOf),
                (("Base", "isSuffixOf"),
                 CustardNativeLambda Lists.cfIsSuffixOf),
                (("Base", "isInfixOf"),
                 CustardNativeLambda Lists.cfIsInfixOf),
                -- Lists - Searching lists
                -- Lists - Searching lists - Searching by equality
                (("Base", "elem"),
                 CustardNativeLambda Lists.cfElem),
                (("Base", "notElem"),
                 CustardNativeLambda Lists.cfNotElem),
                (("Base", "lookup"),
                 CustardNativeLambda Lists.cfLookup),
                -- Lists - Searching lists - Searching with a predicate
                (("Base", "find"),
                 CustardNativeLambda Lists.cfFind),
                (("Base", "filter"),
                 CustardNativeLambda Lists.cfFilter),
                (("Base", "partition"),
                 CustardNativeLambda Lists.cfPartition),
                -- Lists - Indexing lists
                (("Base", "nth"),
                 CustardNativeLambda Lists.cfNth),
                (("Base", "elemIndex"),
                 CustardNativeLambda Lists.cfElemIndex),
                (("Base", "elemIndices"),
                 CustardNativeLambda Lists.cfElemIndices),
                (("Base", "findIndex"),
                 CustardNativeLambda Lists.cfFindIndex),
                (("Base", "findIndices"),
                 CustardNativeLambda Lists.cfFindIndices),
                -- Lists - Special lists
                -- Lists - "Set" operations
                (("Base", "nub"),
                 CustardNativeLambda Lists.cfNub),
                (("Base", "delete"),
                 CustardNativeLambda Lists.cfDelete),
                (("Base", "deleteFirsts"),
                 CustardNativeLambda Lists.cfDeleteFirsts),
                (("Base", "union"),
                 CustardNativeLambda Lists.cfUnion),
                (("Base", "intersect"),
                 CustardNativeLambda Lists.cfIntersect),
                -- Lists - Ordered lists
                (("Base", "sort"),
                 CustardNativeLambda Lists.cfSort),
                (("Base", "insert"),
                 CustardNativeLambda Lists.cfInsert),
                -- Lists - Generalized functions
                (("Base", "nubBy"),
                 CustardNativeLambda Lists.cfNubBy),
                (("Base", "deleteBy"),
                 CustardNativeLambda Lists.cfDeleteBy),
                (("Base", "deleteFirstsBy"),
                 CustardNativeLambda Lists.cfDeleteFirstsBy),
                (("Base", "unionBy"),
                 CustardNativeLambda Lists.cfUnionBy),
                (("Base", "intersectBy"),
                 CustardNativeLambda Lists.cfIntersectBy),
                (("Base", "groupBy"),
                 CustardNativeLambda Lists.cfGroupBy),
                (("Base", "sortBy"),
                 CustardNativeLambda Lists.cfSortBy),
                (("Base", "insertBy"),
                 CustardNativeLambda Lists.cfInsertBy),
                (("Base", "maximumBy"),
                 CustardNativeLambda Lists.cfMaximumBy),
                (("Base", "minimumBy"),
                 CustardNativeLambda Lists.cfMinimumBy),
                
                -- Strings
                -- Strings - Basic functions
                (("Base", "stringHead"),
                 CustardNativeLambda Strings.cfStringHead),
                (("Base", "stringLast"),
                 CustardNativeLambda Strings.cfStringLast),
                (("Base", "stringTail"),
                 CustardNativeLambda Strings.cfStringTail),
                (("Base", "stringInit"),
                 CustardNativeLambda Strings.cfStringInit),
                (("Base", "stringNull"),
                 CustardNativeLambda Strings.cfStringNull),
                (("Base", "stringLength"),
                 CustardNativeLambda Strings.cfStringLength),
                -- Strings - String transformations
                (("Base", "stringMap"),
                 CustardNativeLambda Strings.cfStringMap),
                (("Base", "stringReverse"),
                 CustardNativeLambda Strings.cfStringReverse),
                (("Base", "stringIntersperse"),
                 CustardNativeLambda Strings.cfStringIntersperse),
                (("Base", "stringIntercalate"),
                 CustardNativeLambda Strings.cfStringIntercalate),
                (("Base", "stringTranspose"),
                 CustardNativeLambda Strings.cfStringTranspose),
                (("Base", "stringSubsequences"),
                 CustardNativeLambda Strings.cfStringSubsequences),
                (("Base", "stringPermutations"),
                 CustardNativeLambda Strings.cfStringPermutations),
                -- Strings - Reducing strings (folds)
                (("Base", "stringFoldl"),
                 CustardNativeLambda Strings.cfStringFoldl),
                (("Base", "stringFoldl1"),
                 CustardNativeLambda Strings.cfStringFoldl1),
                (("Base", "stringFoldr"),
                 CustardNativeLambda Strings.cfStringFoldr),
                (("Base", "stringFoldr1"),
                 CustardNativeLambda Strings.cfStringFoldr1),
                -- Strings - Reducing strings (folds) - Special folds
                (("Base", "stringConcat"),
                 CustardNativeLambda Strings.cfStringConcat),
                (("Base", "stringConcatMap"),
                 CustardNativeLambda Strings.cfStringConcatMap),
                (("Base", "stringAny"),
                 CustardNativeLambda Strings.cfStringAny),
                (("Base", "stringAll"),
                 CustardNativeLambda Strings.cfStringAll),
                -- Strings - Building strings
                -- Strings - Building strings - Scans
                (("Base", "stringScanl"),
                 CustardNativeLambda Strings.cfStringScanl),
                (("Base", "stringScanl1"),
                 CustardNativeLambda Strings.cfStringScanl1),
                (("Base", "stringScanr"),
                 CustardNativeLambda Strings.cfStringScanr),
                (("Base", "stringScanr1"),
                 CustardNativeLambda Strings.cfStringScanr1),
                -- Strings - Building strings - Accumulating maps
                (("Base", "stringMapAccumL"),
                 CustardNativeLambda Strings.cfStringMapAccumL),
                (("Base", "stringMapAccumR"),
                 CustardNativeLambda Strings.cfStringMapAccumR),
                -- Strings - Building strings - Replicate
                (("Base", "stringReplicate"),
                 CustardNativeLambda Strings.cfStringReplicate),
                -- Strings - Building strings - Unfolding
                (("Base", "stringUnfoldr"),
                 CustardNativeLambda Strings.cfStringUnfoldr),
                -- Strings - Substrings
                -- Strings - Substrings - Extracting sublists
                (("Base", "stringTake"),
                 CustardNativeLambda Strings.cfStringTake),
                (("Base", "stringDrop"),
                 CustardNativeLambda Strings.cfStringDrop),
                (("Base", "stringSplitAt"),
                 CustardNativeLambda Strings.cfStringSplitAt),
                (("Base", "stringTakeWhile"),
                 CustardNativeLambda Strings.cfStringTakeWhile),
                (("Base", "stringDropWhile"),
                 CustardNativeLambda Strings.cfStringDropWhile),
                (("Base", "stringSpan"),
                 CustardNativeLambda Strings.cfStringSpan),
                (("Base", "stringBreak"),
                 CustardNativeLambda Strings.cfStringBreak),
                (("Base", "stringStripPrefix"),
                 CustardNativeLambda Strings.cfStringStripPrefix),
                (("Base", "stringGroup"),
                 CustardNativeLambda Strings.cfStringGroup),
                (("Base", "stringInits"),
                 CustardNativeLambda Strings.cfStringInits),
                (("Base", "stringTails"),
                 CustardNativeLambda Strings.cfStringTails),
                -- Strings - Substrings - Predicates
                (("Base", "stringIsPrefixOf"),
                 CustardNativeLambda Strings.cfStringIsPrefixOf),
                (("Base", "stringIsSuffixOf"),
                 CustardNativeLambda Strings.cfStringIsSuffixOf),
                (("Base", "stringIsInfixOf"),
                 CustardNativeLambda Strings.cfStringIsInfixOf),
                -- Strings - Searching strings
                -- Strings - Searching strings - Searching by equality
                (("Base", "stringElem"),
                 CustardNativeLambda Strings.cfStringElem),
                (("Base", "stringNotElem"),
                 CustardNativeLambda Strings.cfStringNotElem),
                (("Base", "stringLookup"),
                 CustardNativeLambda Strings.cfStringLookup),
                -- Strings - Searching strings - Searching with a predicate
                (("Base", "stringFind"),
                 CustardNativeLambda Strings.cfStringFind),
                (("Base", "stringFilter"),
                 CustardNativeLambda Strings.cfStringFilter),
                (("Base", "stringPartition"),
                 CustardNativeLambda Strings.cfStringPartition),
                -- Strings - Indexing strings
                (("Base", "stringNth"),
                 CustardNativeLambda Strings.cfStringNth),
                (("Base", "stringElemIndex"),
                 CustardNativeLambda Strings.cfStringElemIndex),
                (("Base", "stringElemIndices"),
                 CustardNativeLambda Strings.cfStringElemIndices),
                (("Base", "stringFindIndex"),
                 CustardNativeLambda Strings.cfStringFindIndex),
                (("Base", "stringFindIndices"),
                 CustardNativeLambda Strings.cfStringFindIndices),
                -- Strings - Text operations
                (("Base", "stringLines"),
                 CustardNativeLambda Strings.cfStringLines),
                (("Base", "stringWords"),
                 CustardNativeLambda Strings.cfStringWords),
                (("Base", "stringUnlines"),
                 CustardNativeLambda Strings.cfStringUnlines),
                (("Base", "stringUnwords"),
                 CustardNativeLambda Strings.cfStringUnwords)
               ]
