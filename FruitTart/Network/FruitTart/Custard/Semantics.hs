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
import qualified Network.FruitTart.Custard.Functions.Symbols as Symbols
import qualified Network.FruitTart.Custard.Functions.HTTP as HTTP
import qualified Network.FruitTart.Custard.Functions.Forms as Forms
import qualified Network.FruitTart.Custard.Functions.Sessions as Sessions
import qualified Network.FruitTart.Custard.Functions.Captchas as Captchas
import qualified Network.FruitTart.Custard.Functions.Lists as Lists
import qualified Network.FruitTart.Custard.Functions.Strings as Strings
import qualified Network.FruitTart.Custard.Functions.Maps as Maps
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
  expression <- fCatch (liftIO $ readExpression database moduleName body)
                       (\e -> error $ "While evaluating expression "
                                      ++ (show body)
                                      ++ ": " ++ (show (e :: SomeException)))
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
      CustardQuoteExpression subexpressions -> do
        if length subexpressions /= 1
           then error $ "Invalid number of parameters to quote()."
           else return ()
        (moduleName, properName)
            <- case head subexpressions of
                 CustardVariable result -> return result
                 _ -> error $ "Parameter to quote() is not a symbol."
        return (context, CustardSymbol moduleName properName)
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
        return (context, CustardLambda Nothing formalParameters bindings body)
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
    Nothing -> do
      maybeValue <- getTopLevelBinding name
      case maybeValue of
        Just value -> do
          let CustardContext {
                        custardContextGlobalBindings = oldBindings
                      } = context
              newBindings = Map.union (Map.fromList [(name, value)])
                                      oldBindings
              context' = context {
                                  custardContextGlobalBindings = newBindings
                                }
          return (context', value)
        Nothing -> error $ "Undefined variable "
                           ++ packageName ++ "."
                           ++ properName ++ "."
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
    CustardLambda maybeVariableName
                  formalParameters
                  capturedBindings
                  body -> do
      if length formalParameters /= length actualParameters
        then do
          let displayName = case maybeVariableName of
                              Nothing -> "anonymous function"
                              Just (moduleName, properName)
                                -> moduleName ++ "." ++ properName
          error $ "Expected " ++ (show $ length formalParameters)
                   ++ " parameters to " ++ displayName
                   ++ ", but got " ++ (show $ length actualParameters) ++ "."
        else return ()
      let newBindings = Map.fromList $ zip (map (\(CustardParameter key) -> key)
                                                formalParameters)
                                           actualParameters
          subbindings = Map.union newBindings capturedBindings
          context' = context { custardContextLexicalBindings = subbindings }
      (outputContext, result)
        <- fCatch (evalExpression context' body)
                  (\e -> do
                     let displayName = case maybeVariableName of
                                         Nothing -> "anonymous function"
                                         Just (moduleName, properName)
                                           -> moduleName ++ "." ++ properName
                     error $ "In " ++ displayName ++ ": "
                             ++ (show (e :: SomeException)))
      let CustardContext { custardContextGlobalBindings = outputBindings }
            = outputContext
          outputContext' = context { custardContextGlobalBindings = outputBindings }
      return (outputContext', result)
    CustardNativeLambda (moduleName, properName) body -> do
      result <- fCatch (body context actualParameters)
                       (\e -> error $ "In builtin "
                                      ++ moduleName ++ "." ++ properName
                                      ++ ": " ++ (show (e :: SomeException)))
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
          compiledBody
            <- fCatch (liftIO $ readExpression database moduleName functionBody)
                      (\e -> error $ "While reading expression "
                                     ++ (show functionBody)
                                     ++ " from body of function "
                                     ++ moduleName ++ "." ++ functionName
                                     ++ ": " ++ (show (e :: SomeException)))
          parameterItems <- query ("SELECT name "
                                   ++ "FROM function_parameters "
                                   ++ "WHERE function = ? "
                                   ++ "ORDER BY item")
                                  [SQLInteger functionID]
          FruitTartState { database = database } <- get
          parameters <- mapM (\[SQLText parameterName] -> do
                               TokenSymbol moduleName parameterName
                                 <- liftIO
                                    $ intern database moduleName parameterName
                               return $ CustardParameter
                                         (moduleName, parameterName))
                             parameterItems
          return $ Just $ CustardLambda (Just variableName)
                                        parameters
                                        Map.empty
                                        compiledBody
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
                 CustardNativeLambda ("Base", "cfJust")
                                     General.cfJust),
                (("Base", "LT"),
                 CustardOrdering LT),
                (("Base", "GT"),
                 CustardOrdering GT),
                (("Base", "EQ"),
                 CustardOrdering EQ),
                (("Base", "parameter"),
                 CustardNativeLambda ("Base", "cfParameter")
                                     General.cfParameter),
                (("Base", "isNothing"),
                 CustardNativeLambda ("Base", "cfIsNothing")
                                     General.cfIsNothing),
                (("Base", "isJust"),
                 CustardNativeLambda ("Base", "cfIsJust")
                                     General.cfIsJust),
                (("Base", "fromJust"),
                 CustardNativeLambda ("Base", "cfFromJust")
                                     General.cfFromJust),
                (("Base", "compareIntegers"),
                 CustardNativeLambda ("Base", "cfCompareIntegers")
                                     General.cfCompareIntegers),
                (("Base", "showInteger"),
                 CustardNativeLambda ("Base", "cfShowInteger")
                                     General.cfShowInteger),
                (("Base", "showBool"),
                 CustardNativeLambda ("Base", "cfShowBool")
                                     General.cfShowBool),
                (("Base", "byteSizeToString"),
                 CustardNativeLambda ("Base", "cfByteSizeToString")
                                     General.cfByteSizeToString),
                (("Base", "timestampToString"),
                 CustardNativeLambda ("Base", "cfTimestampToString")
                                     General.cfTimestampToString),
                (("Base", "escapeAttribute"),
                 CustardNativeLambda ("Base", "cfEscapeAttribute")
                                     General.cfEscapeAttribute),
                (("Base", "escapeHTML"),
                 CustardNativeLambda ("Base", "cfEscapeHTML")
                                     General.cfEscapeHTML),
                (("Base", "newlinesToParagraphs"),
                 CustardNativeLambda ("Base", "cfNewlinesToParagraphs")
                                     General.cfNewlinesToParagraphs),
                
                -- Symbols
                (("Base.Symbols", "symbolName"),
                 CustardNativeLambda ("Base.Symbols", "cfSymbolName")
                                     Symbols.cfSymbolName),
                (("Base.Symbols", "symbolModule"),
                 CustardNativeLambda ("Base.Symbols", "cfSymbolModule")
                                     Symbols.cfSymbolModule),
                (("Base.Symbols", "makeSymbol"),
                 CustardNativeLambda ("Base.Symbols", "cfMakeSymbol")
                                     Symbols.cfMakeSymbol),
                
                -- HTTP
                (("Base.HTTP", "log"),
                 CustardNativeLambda ("Base.HTTP", "cfLog")
                                     HTTP.cfLog),
                (("Base.HTTP", "getRequestVariable"),
                 CustardNativeLambda ("Base.HTTP", "cfGetRequestVariable")
                                     HTTP.cfGetRequestVariable),
                (("Base.HTTP", "getAllRequestVariables"),
                 CustardNativeLambda ("Base.HTTP", "cfGetAllRequestVariables")
                                     HTTP.cfGetAllRequestVariables),
                (("Base.HTTP", "HttpAccept"),
                 CustardHTTPHeader FCGI.HttpAccept),
                (("Base.HTTP", "HttpAcceptCharset"),
                 CustardHTTPHeader FCGI.HttpAcceptCharset),
                (("Base.HTTP", "HttpAcceptEncoding"),
                 CustardHTTPHeader FCGI.HttpAcceptEncoding),
                (("Base.HTTP", "HttpAcceptLanguage"),
                 CustardHTTPHeader FCGI.HttpAcceptLanguage),
                (("Base.HTTP", "HttpAuthorization"),
                 CustardHTTPHeader FCGI.HttpAuthorization),
                (("Base.HTTP", "HttpExpect"),
                 CustardHTTPHeader FCGI.HttpExpect),
                (("Base.HTTP", "HttpFrom"),
                 CustardHTTPHeader FCGI.HttpFrom),
                (("Base.HTTP", "HttpHost"),
                 CustardHTTPHeader FCGI.HttpHost),
                (("Base.HTTP", "HttpIfMatch"),
                 CustardHTTPHeader FCGI.HttpIfMatch),
                (("Base.HTTP", "HttpIfModifiedSince"),
                 CustardHTTPHeader FCGI.HttpIfModifiedSince),
                (("Base.HTTP", "HttpIfNoneMatch"),
                 CustardHTTPHeader FCGI.HttpIfNoneMatch),
                (("Base.HTTP", "HttpIfRange"),
                 CustardHTTPHeader FCGI.HttpIfRange),
                (("Base.HTTP", "HttpIfUnmodifiedSince"),
                 CustardHTTPHeader FCGI.HttpIfUnmodifiedSince),
                (("Base.HTTP", "HttpMaxForwards"),
                 CustardHTTPHeader FCGI.HttpMaxForwards),
                (("Base.HTTP", "HttpProxyAuthorization"),
                 CustardHTTPHeader FCGI.HttpProxyAuthorization),
                (("Base.HTTP", "HttpRange"),
                 CustardHTTPHeader FCGI.HttpRange),
                (("Base.HTTP", "HttpReferer"),
                 CustardHTTPHeader FCGI.HttpReferer),
                (("Base.HTTP", "HttpTE"),
                 CustardHTTPHeader FCGI.HttpTE),
                (("Base.HTTP", "HttpUserAgent"),
                 CustardHTTPHeader FCGI.HttpUserAgent),
                (("Base.HTTP", "HttpAcceptRanges"),
                 CustardHTTPHeader FCGI.HttpAcceptRanges),
                (("Base.HTTP", "HttpAge"),
                 CustardHTTPHeader FCGI.HttpAge),
                (("Base.HTTP", "HttpETag"),
                 CustardHTTPHeader FCGI.HttpETag),
                (("Base.HTTP", "HttpLocation"),
                 CustardHTTPHeader FCGI.HttpLocation),
                (("Base.HTTP", "HttpProxyAuthenticate"),
                 CustardHTTPHeader FCGI.HttpProxyAuthenticate),
                (("Base.HTTP", "HttpRetryAfter"),
                 CustardHTTPHeader FCGI.HttpRetryAfter),
                (("Base.HTTP", "HttpServer"),
                 CustardHTTPHeader FCGI.HttpServer),
                (("Base.HTTP", "HttpVary"),
                 CustardHTTPHeader FCGI.HttpVary),
                (("Base.HTTP", "HttpWWWAuthenticate"),
                 CustardHTTPHeader FCGI.HttpWWWAuthenticate),
                (("Base.HTTP", "HttpAllow"),
                 CustardHTTPHeader FCGI.HttpAllow),
                (("Base.HTTP", "HttpContentEncoding"),
                 CustardHTTPHeader FCGI.HttpContentEncoding),
                (("Base.HTTP", "HttpContentLanguage"),
                 CustardHTTPHeader FCGI.HttpContentLanguage),
                (("Base.HTTP", "HttpContentLength"),
                 CustardHTTPHeader FCGI.HttpContentLength),
                (("Base.HTTP", "HttpContentLocation"),
                 CustardHTTPHeader FCGI.HttpContentLocation),
                (("Base.HTTP", "HttpContentMD5"),
                 CustardHTTPHeader FCGI.HttpContentMD5),
                (("Base.HTTP", "HttpContentRange"),
                 CustardHTTPHeader FCGI.HttpContentRange),
                (("Base.HTTP", "HttpContentType"),
                 CustardHTTPHeader FCGI.HttpContentType),
                (("Base.HTTP", "HttpExpires"),
                 CustardHTTPHeader FCGI.HttpExpires),
                (("Base.HTTP", "HttpLastModified"),
                 CustardHTTPHeader FCGI.HttpLastModified),
                (("Base.HTTP", "HttpConnection"),
                 CustardHTTPHeader FCGI.HttpConnection),
                (("Base.HTTP", "HttpCookie"),
                 CustardHTTPHeader FCGI.HttpCookie),
                (("Base.HTTP", "HttpSetCookie"),
                 CustardHTTPHeader FCGI.HttpSetCookie),
                (("Base.HTTP", "getRequestHeader"),
                 CustardNativeLambda ("Base.HTTP", "cfGetRequestHeader")
                                     HTTP.cfGetRequestHeader),
                (("Base.HTTP", "cookieName"),
                 CustardNativeLambda ("Base.HTTP", "cfCookieName")
                                     HTTP.cfCookieName),
                (("Base.HTTP", "cookieValue"),
                 CustardNativeLambda ("Base.HTTP", "cfCookieValue")
                                     HTTP.cfCookieValue),
                (("Base.HTTP", "cookieVersion"),
                 CustardNativeLambda ("Base.HTTP", "cfCookieVersion")
                                     HTTP.cfCookieVersion),
                (("Base.HTTP", "cookiePath"),
                 CustardNativeLambda ("Base.HTTP", "cfCookiePath")
                                     HTTP.cfCookiePath),
                (("Base.HTTP", "cookieDomain"),
                 CustardNativeLambda ("Base.HTTP", "cfCookieDomain")
                                     HTTP.cfCookieDomain),
                (("Base.HTTP", "cookieMaxAge"),
                 CustardNativeLambda ("Base.HTTP", "cfCookieMaxAge")
                                     HTTP.cfCookieMaxAge),
                (("Base.HTTP", "cookieSecure"),
                 CustardNativeLambda ("Base.HTTP", "cfCookieSecure")
                                     HTTP.cfCookieSecure),
                (("Base.HTTP", "cookieComment"),
                 CustardNativeLambda ("Base.HTTP", "cfCookieComment")
                                     HTTP.cfCookieComment),
                (("Base.HTTP", "getCookie"),
                 CustardNativeLambda ("Base.HTTP", "cfGetCookie")
                                     HTTP.cfGetCookie),
                (("Base.HTTP", "getAllCookies"),
                 CustardNativeLambda ("Base.HTTP", "cfGetAllCookies")
                                     HTTP.cfGetAllCookies),
                (("Base.HTTP", "getCookieValue"),
                 CustardNativeLambda ("Base.HTTP", "cfGetCookieValue")
                                     HTTP.cfGetCookieValue),
                (("Base.HTTP", "getDocumentRoot"),
                 CustardNativeLambda ("Base.HTTP", "cfGetDocumentRoot")
                                     HTTP.cfGetDocumentRoot),
                (("Base.HTTP", "getGatewayInterface"),
                 CustardNativeLambda ("Base.HTTP", "cfGetGatewayInterface")
                                     HTTP.cfGetGatewayInterface),
                (("Base.HTTP", "getPathInfo"),
                 CustardNativeLambda ("Base.HTTP", "cfGetPathInfo")
                                     HTTP.cfGetPathInfo),
                (("Base.HTTP", "getPathTranslated"),
                 CustardNativeLambda ("Base.HTTP", "cfGetPathTranslated")
                                     HTTP.cfGetPathTranslated),
                (("Base.HTTP", "getQueryString"),
                 CustardNativeLambda ("Base.HTTP", "cfGetQueryString")
                                     HTTP.cfGetQueryString),
                (("Base.HTTP", "getRedirectStatus"),
                 CustardNativeLambda ("Base.HTTP", "cfGetRedirectStatus")
                                     HTTP.cfGetRedirectStatus),
                (("Base.HTTP", "getRedirectURI"),
                 CustardNativeLambda ("Base.HTTP", "cfGetRedirectURI")
                                     HTTP.cfGetRedirectURI),
                (("Base.HTTP", "getRemoteAddress"),
                 CustardNativeLambda ("Base.HTTP", "cfGetRemoteAddress")
                                     HTTP.cfGetRemoteAddress),
                (("Base.HTTP", "getRemotePort"),
                 CustardNativeLambda ("Base.HTTP", "cfGetRemotePort")
                                     HTTP.cfGetRemotePort),
                (("Base.HTTP", "getRemoteHost"),
                 CustardNativeLambda ("Base.HTTP", "cfGetRemoteHost")
                                     HTTP.cfGetRemoteHost),
                (("Base.HTTP", "getRemoteIdent"),
                 CustardNativeLambda ("Base.HTTP", "cfGetRemoteIdent")
                                     HTTP.cfGetRemoteIdent),
                (("Base.HTTP", "getRemoteUser"),
                 CustardNativeLambda ("Base.HTTP", "cfGetRemoteUser")
                                     HTTP.cfGetRemoteUser),
                (("Base.HTTP", "getRequestMethod"),
                 CustardNativeLambda ("Base.HTTP", "cfGetRequestMethod")
                                     HTTP.cfGetRequestMethod),
                (("Base.HTTP", "getRequestURI"),
                 CustardNativeLambda ("Base.HTTP", "cfGetRequestURI")
                                     HTTP.cfGetRequestURI),
                (("Base.HTTP", "getScriptFilename"),
                 CustardNativeLambda ("Base.HTTP", "cfGetScriptFilename")
                                     HTTP.cfGetScriptFilename),
                (("Base.HTTP", "getScriptName"),
                 CustardNativeLambda ("Base.HTTP", "cfGetScriptName")
                                     HTTP.cfGetScriptName),
                (("Base.HTTP", "getServerAddress"),
                 CustardNativeLambda ("Base.HTTP", "cfGetServerAddress")
                                     HTTP.cfGetServerAddress),
                (("Base.HTTP", "getServerName"),
                 CustardNativeLambda ("Base.HTTP", "cfGetServerName")
                                     HTTP.cfGetServerName),
                (("Base.HTTP", "getServerPort"),
                 CustardNativeLambda ("Base.HTTP", "cfGetServerPort")
                                     HTTP.cfGetServerPort),
                (("Base.HTTP", "getServerProtocol"),
                 CustardNativeLambda ("Base.HTTP", "cfGetServerProtocol")
                                     HTTP.cfGetServerProtocol),
                (("Base.HTTP", "getServerSoftware"),
                 CustardNativeLambda ("Base.HTTP", "cfGetServerSoftware")
                                     HTTP.cfGetServerSoftware),
                (("Base.HTTP", "getAuthenticationType"),
                 CustardNativeLambda ("Base.HTTP", "cfGetAuthenticationType")
                                     HTTP.cfGetAuthenticationType),
                (("Base.HTTP", "getContentLength"),
                 CustardNativeLambda ("Base.HTTP", "cfGetContentLength")
                                     HTTP.cfGetContentLength),
                (("Base.HTTP", "getContentType"),
                 CustardNativeLambda ("Base.HTTP", "cfGetContentType")
                                     HTTP.cfGetContentType),
                (("Base.HTTP", "setResponseStatus"),
                 CustardNativeLambda ("Base.HTTP", "cfSetResponseStatus")
                                     HTTP.cfSetResponseStatus),
                (("Base.HTTP", "getResponseStatus"),
                 CustardNativeLambda ("Base.HTTP", "cfGetResponseStatus")
                                     HTTP.cfGetResponseStatus),
                (("Base.HTTP", "setResponseHeader"),
                 CustardNativeLambda ("Base.HTTP", "cfSetResponseHeader")
                                     HTTP.cfSetResponseHeader),
                (("Base.HTTP", "unsetResponseHeader"),
                 CustardNativeLambda ("Base.HTTP", "cfUnsetResponseHeader")
                                     HTTP.cfUnsetResponseHeader),
                (("Base.HTTP", "getResponseHeader"),
                 CustardNativeLambda ("Base.HTTP", "cfGetResponseHeader")
                                     HTTP.cfGetResponseHeader),
                (("Base.HTTP", "setCookie"),
                 CustardNativeLambda ("Base.HTTP", "cfSetCookie")
                                     HTTP.cfSetCookie),
                (("Base.HTTP", "unsetCookie"),
                 CustardNativeLambda ("Base.HTTP", "cfUnsetCookie")
                                     HTTP.cfUnsetCookie),
                (("Base.HTTP", "makeSimpleCookie"),
                 CustardNativeLambda ("Base.HTTP", "cfMakeSimpleCookie")
                                     HTTP.cfMakeSimpleCookie),
                (("Base.HTTP", "makeCookie"),
                 CustardNativeLambda ("Base.HTTP", "cfMakeCookie")
                                     HTTP.cfMakeCookie),
                (("Base.HTTP", "permanentRedirect"),
                 CustardNativeLambda ("Base.HTTP", "cfPermanentRedirect")
                                     HTTP.cfPermanentRedirect),
                (("Base.HTTP", "seeOtherRedirect"),
                 CustardNativeLambda ("Base.HTTP", "cfSeeOtherRedirect")
                                     HTTP.cfSeeOtherRedirect),
                (("Base.HTTP", "sendResponseHeaders"),
                 CustardNativeLambda ("Base.HTTP", "cfSendResponseHeaders")
                                     HTTP.cfSendResponseHeaders),
                (("Base.HTTP", "responseHeadersSent"),
                 CustardNativeLambda ("Base.HTTP", "cfResponseHeadersSent")
                                     HTTP.cfResponseHeadersSent),
                (("Base.HTTP", "put"),
                 CustardNativeLambda ("Base.HTTP", "cfPut")
                                     HTTP.cfPut),
                (("Base.HTTP", "putString"),
                 CustardNativeLambda ("Base.HTTP", "cfPutString")
                                     HTTP.cfPutString),
                (("Base.HTTP", "closeOutput"),
                 CustardNativeLambda ("Base.HTTP", "cfCloseOutput")
                                     HTTP.cfCloseOutput),
                
                -- Forms
                (("Base.Forms", "formInput"),
                 CustardNativeLambda ("Base.Forms", "cfFormInput")
                                     Forms.cfFormInput),
                
                -- Sessions
                (("Base.Sessions", "getSessionID"),
                 CustardNativeLambda ("Base.Sessions", "cfGetSessionID")
                                     Sessions.cfGetSessionID),
                
                -- Captchas
                (("Base.Captchas", "generateCaptcha"),
                 CustardNativeLambda ("Base.Captchas", "cfGenerateCaptcha")
                                     Captchas.cfGenerateCaptcha),
                (("Base.Captchas", "lookupCaptcha"),
                 CustardNativeLambda ("Base.Captchas", "cfLookupCaptcha")
                                     Captchas.cfLookupCaptcha),
                (("Base.Captchas", "checkCaptcha"),
                 CustardNativeLambda ("Base.Captchas", "cfCheckCaptcha")
                                     Captchas.cfCheckCaptcha),
                (("Base.Captchas", "expireOldCaptchas"),
                 CustardNativeLambda ("Base.Captchas", "cfExpireOldCaptchas")
                                     Captchas.cfExpireOldCaptchas),
                
                -- Everything below here is based on a corresponding function
                -- in Haskell, which may or may not have an identical name.
                
                -- Lists
                -- Lists - Basic functions
                (("Base.Lists", "head"),
                 CustardNativeLambda ("Base.Lists", "cfHead")
                                     Lists.cfHead),
                (("Base.Lists", "last"),
                 CustardNativeLambda ("Base.Lists", "cfLast")
                                     Lists.cfLast),
                (("Base.Lists", "tail"),
                 CustardNativeLambda ("Base.Lists", "cfTail")
                                     Lists.cfTail),
                (("Base.Lists", "init"),
                 CustardNativeLambda ("Base.Lists", "cfInit")
                                     Lists.cfInit),
                (("Base.Lists", "null"),
                 CustardNativeLambda ("Base.Lists", "cfNull")
                                     Lists.cfNull),
                (("Base.Lists", "length"),
                 CustardNativeLambda ("Base.Lists", "cfLength")
                                     Lists.cfLength),
                -- Lists - List transformations
                (("Base.Lists", "map"),
                 CustardNativeLambda ("Base.Lists", "cfMap")
                                     Lists.cfMap),
                (("Base.Lists", "reverse"),
                 CustardNativeLambda ("Base.Lists", "cfReverse")
                                     Lists.cfReverse),
                (("Base.Lists", "intersperse"),
                 CustardNativeLambda ("Base.Lists", "cfIntersperse")
                                     Lists.cfIntersperse),
                (("Base.Lists", "intercalate"),
                 CustardNativeLambda ("Base.Lists", "cfIntercalate")
                                     Lists.cfIntercalate),
                (("Base.Lists", "transpose"),
                 CustardNativeLambda ("Base.Lists", "cfTranspose")
                                     Lists.cfTranspose),
                (("Base.Lists", "subsequences"),
                 CustardNativeLambda ("Base.Lists", "cfSubsequences")
                                     Lists.cfSubsequences),
                (("Base.Lists", "permutations"),
                 CustardNativeLambda ("Base.Lists", "cfPermutations")
                                     Lists.cfPermutations),
                -- Lists - Reducing lists (folds)
                (("Base.Lists", "foldl"),
                 CustardNativeLambda ("Base.Lists", "cfFoldl")
                                     Lists.cfFoldl),
                (("Base.Lists", "foldl1"),
                 CustardNativeLambda ("Base.Lists", "cfFoldl1")
                                     Lists.cfFoldl1),
                (("Base.Lists", "foldr"),
                 CustardNativeLambda ("Base.Lists", "cfFoldr")
                                     Lists.cfFoldr),
                (("Base.Lists", "foldr1"),
                 CustardNativeLambda ("Base.Lists", "cfFoldr1")
                                     Lists.cfFoldr1),
                -- Lists - Reducing lists (folds) - Special folds
                (("Base.Lists", "concat"),
                 CustardNativeLambda ("Base.Lists", "cfConcat")
                                     Lists.cfConcat),
                (("Base.Lists", "concatMap"),
                 CustardNativeLambda ("Base.Lists", "cfConcatMap")
                                     Lists.cfConcatMap),
                (("Base.Lists", "and"),
                 CustardNativeLambda ("Base.Lists", "cfAnd")
                                     Lists.cfAnd),
                (("Base.Lists", "or"),
                 CustardNativeLambda ("Base.Lists", "cfOr")
                                     Lists.cfOr),
                (("Base.Lists", "any"),
                 CustardNativeLambda ("Base.Lists", "cfAny")
                                     Lists.cfAny),
                (("Base.Lists", "all"),
                 CustardNativeLambda ("Base.Lists", "cfAll")
                                     Lists.cfAll),
                (("Base.Lists", "sum"),
                 CustardNativeLambda ("Base.Lists", "cfSum")
                                     Lists.cfSum),
                (("Base.Lists", "product"),
                 CustardNativeLambda ("Base.Lists", "cfProduct")
                                     Lists.cfProduct),
                (("Base.Lists", "maximum"),
                 CustardNativeLambda ("Base.Lists", "cfMaximum")
                                     Lists.cfMaximum),
                (("Base.Lists", "minimum"),
                 CustardNativeLambda ("Base.Lists", "cfMinimum")
                                     Lists.cfMinimum),
                -- Lists - Building lists
                -- Lists - Building lists - Scans
                (("Base.Lists", "scanl"),
                 CustardNativeLambda ("Base.Lists", "cfScanl")
                                     Lists.cfScanl),
                (("Base.Lists", "scanl1"),
                 CustardNativeLambda ("Base.Lists", "cfScanl1")
                                     Lists.cfScanl1),
                (("Base.Lists", "scanr"),
                 CustardNativeLambda ("Base.Lists", "cfScanr")
                                     Lists.cfScanr),
                (("Base.Lists", "scanr1"),
                 CustardNativeLambda ("Base.Lists", "cfScanr1")
                                     Lists.cfScanr1),
                -- Lists - Building lists - Accumulating maps
                (("Base.Lists", "mapAccumL"),
                 CustardNativeLambda ("Base.Lists", "cfMapAccumL")
                                     Lists.cfMapAccumL),
                (("Base.Lists", "mapAccumR"),
                 CustardNativeLambda ("Base.Lists", "cfMapAccumR")
                                     Lists.cfMapAccumR),
                -- Lists - Building lists - Replicate
                (("Base.Lists", "replicate"),
                 CustardNativeLambda ("Base.Lists", "cfReplicate")
                                     Lists.cfReplicate),
                -- Lists - Building lists - Unfolding
                (("Base.Lists", "unfoldr"),
                 CustardNativeLambda ("Base.Lists", "cfUnfoldr")
                                     Lists.cfUnfoldr),
                -- Lists - Sublists
                -- Lists - Sublists - Extracting sublists
                (("Base.Lists", "take"),
                 CustardNativeLambda ("Base.Lists", "cfTake")
                                     Lists.cfTake),
                (("Base.Lists", "drop"),
                 CustardNativeLambda ("Base.Lists", "cfDrop")
                                     Lists.cfDrop),
                (("Base.Lists", "splitAt"),
                 CustardNativeLambda ("Base.Lists", "cfSplitAt")
                                     Lists.cfSplitAt),
                (("Base.Lists", "takeWhile"),
                 CustardNativeLambda ("Base.Lists", "cfTakeWhile")
                                     Lists.cfTakeWhile),
                (("Base.Lists", "dropWhile"),
                 CustardNativeLambda ("Base.Lists", "cfDropWhile")
                                     Lists.cfDropWhile),
                (("Base.Lists", "span"),
                 CustardNativeLambda ("Base.Lists", "cfSpan")
                                     Lists.cfSpan),
                (("Base.Lists", "break"),
                 CustardNativeLambda ("Base.Lists", "cfBreak")
                                     Lists.cfBreak),
                (("Base.Lists", "stripPrefix"),
                 CustardNativeLambda ("Base.Lists", "cfStripPrefix")
                                     Lists.cfStripPrefix),
                (("Base.Lists", "group"),
                 CustardNativeLambda ("Base.Lists", "cfGroup")
                                     Lists.cfGroup),
                (("Base.Lists", "inits"),
                 CustardNativeLambda ("Base.Lists", "cfInits")
                                     Lists.cfInits),
                (("Base.Lists", "tails"),
                 CustardNativeLambda ("Base.Lists", "cfTails")
                                     Lists.cfTails),
                -- Lists - Sublists - Predicates
                (("Base.Lists", "isPrefixOf"),
                 CustardNativeLambda ("Base.Lists", "cfIsPrefixOf")
                                     Lists.cfIsPrefixOf),
                (("Base.Lists", "isSuffixOf"),
                 CustardNativeLambda ("Base.Lists", "cfIsSuffixOf")
                                     Lists.cfIsSuffixOf),
                (("Base.Lists", "isInfixOf"),
                 CustardNativeLambda ("Base.Lists", "cfIsInfixOf")
                                     Lists.cfIsInfixOf),
                -- Lists - Searching lists
                -- Lists - Searching lists - Searching by equality
                (("Base.Lists", "elem"),
                 CustardNativeLambda ("Base.Lists", "cfElem")
                                     Lists.cfElem),
                (("Base.Lists", "notElem"),
                 CustardNativeLambda ("Base.Lists", "cfNotElem")
                                     Lists.cfNotElem),
                (("Base.Lists", "lookup"),
                 CustardNativeLambda ("Base.Lists", "cfLookup")
                                     Lists.cfLookup),
                -- Lists - Searching lists - Searching with a predicate
                (("Base.Lists", "find"),
                 CustardNativeLambda ("Base.Lists", "cfFind")
                                     Lists.cfFind),
                (("Base.Lists", "filter"),
                 CustardNativeLambda ("Base.Lists", "cfFilter")
                                     Lists.cfFilter),
                (("Base.Lists", "partition"),
                 CustardNativeLambda ("Base.Lists", "cfPartition")
                                     Lists.cfPartition),
                -- Lists - Indexing lists
                (("Base.Lists", "nth"),
                 CustardNativeLambda ("Base.Lists", "cfNth")
                                     Lists.cfNth),
                (("Base.Lists", "elemIndex"),
                 CustardNativeLambda ("Base.Lists", "cfElemIndex")
                                     Lists.cfElemIndex),
                (("Base.Lists", "elemIndices"),
                 CustardNativeLambda ("Base.Lists", "cfElemIndices")
                                     Lists.cfElemIndices),
                (("Base.Lists", "findIndex"),
                 CustardNativeLambda ("Base.Lists", "cfFindIndex")
                                     Lists.cfFindIndex),
                (("Base.Lists", "findIndices"),
                 CustardNativeLambda ("Base.Lists", "cfFindIndices")
                                     Lists.cfFindIndices),
                -- Lists - Special lists
                -- Lists - "Set" operations
                (("Base.Lists", "nub"),
                 CustardNativeLambda ("Base.Lists", "cfNub")
                                     Lists.cfNub),
                (("Base.Lists", "delete"),
                 CustardNativeLambda ("Base.Lists", "cfDelete")
                                     Lists.cfDelete),
                (("Base.Lists", "deleteFirsts"),
                 CustardNativeLambda ("Base.Lists", "cfDeleteFirsts")
                                     Lists.cfDeleteFirsts),
                (("Base.Lists", "union"),
                 CustardNativeLambda ("Base.Lists", "cfUnion")
                                     Lists.cfUnion),
                (("Base.Lists", "intersect"),
                 CustardNativeLambda ("Base.Lists", "cfIntersect")
                                     Lists.cfIntersect),
                -- Lists - Ordered lists
                (("Base.Lists", "sort"),
                 CustardNativeLambda ("Base.Lists", "cfSort")
                                     Lists.cfSort),
                (("Base.Lists", "insert"),
                 CustardNativeLambda ("Base.Lists", "cfInsert")
                                     Lists.cfInsert),
                -- Lists - Generalized functions
                (("Base.Lists", "nubBy"),
                 CustardNativeLambda ("Base.Lists", "cfNubBy")
                                     Lists.cfNubBy),
                (("Base.Lists", "deleteBy"),
                 CustardNativeLambda ("Base.Lists", "cfDeleteBy")
                                     Lists.cfDeleteBy),
                (("Base.Lists", "deleteFirstsBy"),
                 CustardNativeLambda ("Base.Lists", "cfDeleteFirstsBy")
                                     Lists.cfDeleteFirstsBy),
                (("Base.Lists", "unionBy"),
                 CustardNativeLambda ("Base.Lists", "cfUnionBy")
                                     Lists.cfUnionBy),
                (("Base.Lists", "intersectBy"),
                 CustardNativeLambda ("Base.Lists", "cfIntersectBy")
                                     Lists.cfIntersectBy),
                (("Base.Lists", "groupBy"),
                 CustardNativeLambda ("Base.Lists", "cfGroupBy")
                                     Lists.cfGroupBy),
                (("Base.Lists", "sortBy"),
                 CustardNativeLambda ("Base.Lists", "cfSortBy")
                                     Lists.cfSortBy),
                (("Base.Lists", "insertBy"),
                 CustardNativeLambda ("Base.Lists", "cfInsertBy")
                                     Lists.cfInsertBy),
                (("Base.Lists", "maximumBy"),
                 CustardNativeLambda ("Base.Lists", "cfMaximumBy")
                                     Lists.cfMaximumBy),
                (("Base.Lists", "minimumBy"),
                 CustardNativeLambda ("Base.Lists", "cfMinimumBy")
                                     Lists.cfMinimumBy),
                
                -- Strings
                -- Strings - Basic functions
                (("Base.Strings", "stringHead"),
                 CustardNativeLambda ("Base.Strings", "cfStringHead")
                                     Strings.cfStringHead),
                (("Base.Strings", "stringLast"),
                 CustardNativeLambda ("Base.Strings", "cfStringLast")
                                     Strings.cfStringLast),
                (("Base.Strings", "stringTail"),
                 CustardNativeLambda ("Base.Strings", "cfStringTail")
                                     Strings.cfStringTail),
                (("Base.Strings", "stringInit"),
                 CustardNativeLambda ("Base.Strings", "cfStringInit")
                                     Strings.cfStringInit),
                (("Base.Strings", "stringNull"),
                 CustardNativeLambda ("Base.Strings", "cfStringNull")
                                     Strings.cfStringNull),
                (("Base.Strings", "stringLength"),
                 CustardNativeLambda ("Base.Strings", "cfStringLength")
                                     Strings.cfStringLength),
                -- Strings - String transformations
                (("Base.Strings", "stringMap"),
                 CustardNativeLambda ("Base.Strings", "cfStringMap")
                                     Strings.cfStringMap),
                (("Base.Strings", "stringReverse"),
                 CustardNativeLambda ("Base.Strings", "cfStringReverse")
                                     Strings.cfStringReverse),
                (("Base.Strings", "stringIntersperse"),
                 CustardNativeLambda ("Base.Strings", "cfStringIntersperse")
                                     Strings.cfStringIntersperse),
                (("Base.Strings", "stringIntercalate"),
                 CustardNativeLambda ("Base.Strings", "cfStringIntercalate")
                                     Strings.cfStringIntercalate),
                (("Base.Strings", "stringTranspose"),
                 CustardNativeLambda ("Base.Strings", "cfStringTranspose")
                                     Strings.cfStringTranspose),
                (("Base.Strings", "stringSubsequences"),
                 CustardNativeLambda ("Base.Strings", "cfStringSubsequences")
                                     Strings.cfStringSubsequences),
                (("Base.Strings", "stringPermutations"),
                 CustardNativeLambda ("Base.Strings", "cfStringPermutations")
                                     Strings.cfStringPermutations),
                -- Strings - Reducing strings (folds)
                (("Base.Strings", "stringFoldl"),
                 CustardNativeLambda ("Base.Strings", "cfStringFoldl")
                                     Strings.cfStringFoldl),
                (("Base.Strings", "stringFoldl1"),
                 CustardNativeLambda ("Base.Strings", "cfStringFoldl1")
                                     Strings.cfStringFoldl1),
                (("Base.Strings", "stringFoldr"),
                 CustardNativeLambda ("Base.Strings", "cfStringFoldr")
                                     Strings.cfStringFoldr),
                (("Base.Strings", "stringFoldr1"),
                 CustardNativeLambda ("Base.Strings", "cfStringFoldr1")
                                     Strings.cfStringFoldr1),
                -- Strings - Reducing strings (folds) - Special folds
                (("Base.Strings", "stringConcat"),
                 CustardNativeLambda ("Base.Strings", "cfStringConcat")
                                     Strings.cfStringConcat),
                (("Base.Strings", "stringConcatMap"),
                 CustardNativeLambda ("Base.Strings", "cfStringConcatMap")
                                     Strings.cfStringConcatMap),
                (("Base.Strings", "stringAny"),
                 CustardNativeLambda ("Base.Strings", "cfStringAny")
                                     Strings.cfStringAny),
                (("Base.Strings", "stringAll"),
                 CustardNativeLambda ("Base.Strings", "cfStringAll")
                                     Strings.cfStringAll),
                -- Strings - Building strings
                -- Strings - Building strings - Scans
                (("Base.Strings", "stringScanl"),
                 CustardNativeLambda ("Base.Strings", "cfStringScanl")
                                     Strings.cfStringScanl),
                (("Base.Strings", "stringScanl1"),
                 CustardNativeLambda ("Base.Strings", "cfStringScanl1")
                                     Strings.cfStringScanl1),
                (("Base.Strings", "stringScanr"),
                 CustardNativeLambda ("Base.Strings", "cfStringScanr")
                                     Strings.cfStringScanr),
                (("Base.Strings", "stringScanr1"),
                 CustardNativeLambda ("Base.Strings", "cfStringScanr1")
                                     Strings.cfStringScanr1),
                -- Strings - Building strings - Accumulating maps
                (("Base.Strings", "stringMapAccumL"),
                 CustardNativeLambda ("Base.Strings", "cfStringMapAccumL")
                                     Strings.cfStringMapAccumL),
                (("Base.Strings", "stringMapAccumR"),
                 CustardNativeLambda ("Base.Strings", "cfStringMapAccumR")
                                     Strings.cfStringMapAccumR),
                -- Strings - Building strings - Replicate
                (("Base.Strings", "stringReplicate"),
                 CustardNativeLambda ("Base.Strings", "cfStringReplicate")
                                     Strings.cfStringReplicate),
                -- Strings - Building strings - Unfolding
                (("Base.Strings", "stringUnfoldr"),
                 CustardNativeLambda ("Base.Strings", "cfStringUnfoldr")
                                     Strings.cfStringUnfoldr),
                -- Strings - Substrings
                -- Strings - Substrings - Extracting sublists
                (("Base.Strings", "stringTake"),
                 CustardNativeLambda ("Base.Strings", "cfStringTake")
                                     Strings.cfStringTake),
                (("Base.Strings", "stringDrop"),
                 CustardNativeLambda ("Base.Strings", "cfStringDrop")
                                     Strings.cfStringDrop),
                (("Base.Strings", "stringSplitAt"),
                 CustardNativeLambda ("Base.Strings", "cfStringSplitAt")
                                     Strings.cfStringSplitAt),
                (("Base.Strings", "stringTakeWhile"),
                 CustardNativeLambda ("Base.Strings", "cfStringTakeWhile")
                                     Strings.cfStringTakeWhile),
                (("Base.Strings", "stringDropWhile"),
                 CustardNativeLambda ("Base.Strings", "cfStringDropWhile")
                                     Strings.cfStringDropWhile),
                (("Base.Strings", "stringSpan"),
                 CustardNativeLambda ("Base.Strings", "cfStringSpan")
                                     Strings.cfStringSpan),
                (("Base.Strings", "stringBreak"),
                 CustardNativeLambda ("Base.Strings", "cfStringBreak")
                                     Strings.cfStringBreak),
                (("Base.Strings", "stringStripPrefix"),
                 CustardNativeLambda ("Base.Strings", "cfStringStripPrefix")
                                     Strings.cfStringStripPrefix),
                (("Base.Strings", "stringGroup"),
                 CustardNativeLambda ("Base.Strings", "cfStringGroup")
                                     Strings.cfStringGroup),
                (("Base.Strings", "stringInits"),
                 CustardNativeLambda ("Base.Strings", "cfStringInits")
                                     Strings.cfStringInits),
                (("Base.Strings", "stringTails"),
                 CustardNativeLambda ("Base.Strings", "cfStringTails")
                                     Strings.cfStringTails),
                -- Strings - Substrings - Predicates
                (("Base.Strings", "stringIsPrefixOf"),
                 CustardNativeLambda ("Base.Strings", "cfStringIsPrefixOf")
                                     Strings.cfStringIsPrefixOf),
                (("Base.Strings", "stringIsSuffixOf"),
                 CustardNativeLambda ("Base.Strings", "cfStringIsSuffixOf")
                                     Strings.cfStringIsSuffixOf),
                (("Base.Strings", "stringIsInfixOf"),
                 CustardNativeLambda ("Base.Strings", "cfStringIsInfixOf")
                                     Strings.cfStringIsInfixOf),
                -- Strings - Searching strings
                -- Strings - Searching strings - Searching by equality
                (("Base.Strings", "stringElem"),
                 CustardNativeLambda ("Base.Strings", "cfStringElem")
                                     Strings.cfStringElem),
                (("Base.Strings", "stringNotElem"),
                 CustardNativeLambda ("Base.Strings", "cfStringNotElem")
                                     Strings.cfStringNotElem),
                (("Base.Strings", "stringLookup"),
                 CustardNativeLambda ("Base.Strings", "cfStringLookup")
                                     Strings.cfStringLookup),
                -- Strings - Searching strings - Searching with a predicate
                (("Base.Strings", "stringFind"),
                 CustardNativeLambda ("Base.Strings", "cfStringFind")
                                     Strings.cfStringFind),
                (("Base.Strings", "stringFilter"),
                 CustardNativeLambda ("Base.Strings", "cfStringFilter")
                                     Strings.cfStringFilter),
                (("Base.Strings", "stringPartition"),
                 CustardNativeLambda ("Base.Strings", "cfStringPartition")
                                     Strings.cfStringPartition),
                -- Strings - Indexing strings
                (("Base.Strings", "stringNth"),
                 CustardNativeLambda ("Base.Strings", "cfStringNth")
                                     Strings.cfStringNth),
                (("Base.Strings", "stringElemIndex"),
                 CustardNativeLambda ("Base.Strings", "cfStringElemIndex")
                                     Strings.cfStringElemIndex),
                (("Base.Strings", "stringElemIndices"),
                 CustardNativeLambda ("Base.Strings", "cfStringElemIndices")
                                     Strings.cfStringElemIndices),
                (("Base.Strings", "stringFindIndex"),
                 CustardNativeLambda ("Base.Strings", "cfStringFindIndex")
                                     Strings.cfStringFindIndex),
                (("Base.Strings", "stringFindIndices"),
                 CustardNativeLambda ("Base.Strings", "cfStringFindIndices")
                                     Strings.cfStringFindIndices),
                -- Strings - Text operations
                (("Base.Strings", "stringLines"),
                 CustardNativeLambda ("Base.Strings", "cfStringLines")
                                     Strings.cfStringLines),
                (("Base.Strings", "stringWords"),
                 CustardNativeLambda ("Base.Strings", "cfStringWords")
                                     Strings.cfStringWords),
                (("Base.Strings", "stringUnlines"),
                 CustardNativeLambda ("Base.Strings", "cfStringUnlines")
                                     Strings.cfStringUnlines),
                (("Base.Strings", "stringUnwords"),
                 CustardNativeLambda ("Base.Strings", "cfStringUnwords")
                                     Strings.cfStringUnwords),
                
                -- Maps
                -- Maps - Query
                (("Base.Maps", "mapNull"),
                 CustardNativeLambda ("Base.Maps", "cfMapNull")
                                     Maps.cfMapNull),
                (("Base.Maps", "mapSize"),
                 CustardNativeLambda ("Base.Maps", "cfMapSize")
                                     Maps.cfMapSize),
                (("Base.Maps", "mapMember"),
                 CustardNativeLambda ("Base.Maps", "cfMapMember")
                                     Maps.cfMapMember),
                (("Base.Maps", "mapNotMember"),
                 CustardNativeLambda ("Base.Maps", "cfMapNotMember")
                                     Maps.cfMapNotMember),
                (("Base.Maps", "mapLookup"),
                 CustardNativeLambda ("Base.Maps", "cfMapLookup")
                                     Maps.cfMapLookup),
                (("Base.Maps", "mapFindWithDefault"),
                 CustardNativeLambda ("Base.Maps", "cfMapFindWithDefault")
                                     Maps.cfMapFindWithDefault),
                -- Maps - Construction
                (("Base.Maps", "makeEmptyMap"),
                 CustardNativeLambda ("Base.Maps", "cfMakeEmptyMap")
                                     Maps.cfMakeEmptyMap),
                (("Base.Maps", "makeSingletonMap"),
                 CustardNativeLambda ("Base.Maps", "cfMakeSingletonMap")
                                     Maps.cfMakeSingletonMap),
                -- Maps - Construction - Insertion
                (("Base.Maps", "mapInsert"),
                 CustardNativeLambda ("Base.Maps", "cfMapInsert")
                                     Maps.cfMapInsert),
                (("Base.Maps", "mapInsertWith"),
                 CustardNativeLambda ("Base.Maps", "cfMapInsertWith")
                                     Maps.cfMapInsertWith),
                (("Base.Maps", "mapInsertWithKey"),
                 CustardNativeLambda ("Base.Maps", "cfMapInsertWithKey")
                                     Maps.cfMapInsertWithKey),
                (("Base.Maps", "mapInsertLookupWithKey"),
                 CustardNativeLambda ("Base.Maps", "cfMapInsertLookupWithKey")
                                     Maps.cfMapInsertLookupWithKey),
                -- Maps - Construction - Delete/Update
                (("Base.Maps", "mapDelete"),
                 CustardNativeLambda ("Base.Maps", "cfMapDelete")
                                     Maps.cfMapDelete),
                (("Base.Maps", "mapAdjust"),
                 CustardNativeLambda ("Base.Maps", "cfMapAdjust")
                                     Maps.cfMapAdjust),
                (("Base.Maps", "mapAdjustWithKey"),
                 CustardNativeLambda ("Base.Maps", "cfMapAdjustWithKey")
                                     Maps.cfMapAdjustWithKey),
                (("Base.Maps", "mapUpdate"),
                 CustardNativeLambda ("Base.Maps", "cfMapUpdate")
                                     Maps.cfMapUpdate),
                (("Base.Maps", "mapUpdateWithKey"),
                 CustardNativeLambda ("Base.Maps", "cfMapUpdateWithKey")
                                     Maps.cfMapUpdateWithKey),
                (("Base.Maps", "mapUpdateLookupWithKey"),
                 CustardNativeLambda ("Base.Maps", "cfMapUpdateLookupWithKey")
                                     Maps.cfMapUpdateLookupWithKey),
                (("Base.Maps", "mapAlter"),
                 CustardNativeLambda ("Base.Maps", "cfMapAlter")
                                     Maps.cfMapAlter),
                -- Maps - Combine
                -- Maps - Combine - Union
                (("Base.Maps", "mapUnion"),
                 CustardNativeLambda ("Base.Maps", "cfMapUnion")
                                     Maps.cfMapUnion),
                (("Base.Maps", "mapUnionWith"),
                 CustardNativeLambda ("Base.Maps", "cfMapUnionWith")
                                     Maps.cfMapUnionWith),
                (("Base.Maps", "mapUnionWithKey"),
                 CustardNativeLambda ("Base.Maps", "cfMapUnionWithKey")
                                     Maps.cfMapUnionWithKey),
                (("Base.Maps", "mapUnions"),
                 CustardNativeLambda ("Base.Maps", "cfMapUnions")
                                     Maps.cfMapUnions),
                (("Base.Maps", "mapUnionsWith"),
                 CustardNativeLambda ("Base.Maps", "cfMapUnionsWith")
                                     Maps.cfMapUnionsWith),
                -- Maps - Combine - Difference
                (("Base.Maps", "mapDifference"),
                 CustardNativeLambda ("Base.Maps", "cfMapDifference")
                                     Maps.cfMapDifference),
                (("Base.Maps", "mapDifferenceWith"),
                 CustardNativeLambda ("Base.Maps", "cfMapDifferenceWith")
                                     Maps.cfMapDifferenceWith),
                (("Base.Maps", "mapDifferenceWithKey"),
                 CustardNativeLambda ("Base.Maps", "cfMapDifferenceWithKey")
                                     Maps.cfMapDifferenceWithKey),
                -- Maps - Combine - Intersection
                (("Base.Maps", "mapIntersection"),
                 CustardNativeLambda ("Base.Maps", "cfMapIntersection")
                                     Maps.cfMapIntersection),
                (("Base.Maps", "mapIntersectionWith"),
                 CustardNativeLambda ("Base.Maps", "cfMapIntersectionWith")
                                     Maps.cfMapIntersectionWith),
                (("Base.Maps", "mapIntersectionWithKey"),
                 CustardNativeLambda ("Base.Maps", "cfMapIntersectionWithKey")
                                     Maps.cfMapIntersectionWithKey),
                -- Maps - Traversal
                -- Maps - Traversal - Map
                (("Base.Maps", "mapMap"),
                 CustardNativeLambda ("Base.Maps", "cfMapMap")
                                     Maps.cfMapMap),
                (("Base.Maps", "mapMapWithKey"),
                 CustardNativeLambda ("Base.Maps", "cfMapMapWithKey")
                                     Maps.cfMapMapWithKey),
                (("Base.Maps", "mapMapAccum"),
                 CustardNativeLambda ("Base.Maps", "cfMapMapAccum")
                                     Maps.cfMapMapAccum),
                (("Base.Maps", "mapMapAccumWithKey"),
                 CustardNativeLambda ("Base.Maps", "cfMapMapAccumWithKey")
                                     Maps.cfMapMapAccumWithKey),
                (("Base.Maps", "mapMapAccumRWithKey"),
                 CustardNativeLambda ("Base.Maps", "cfMapMapAccumRWithKey")
                                     Maps.cfMapMapAccumRWithKey),
                (("Base.Maps", "mapMapKeys"),
                 CustardNativeLambda ("Base.Maps", "cfMapMapKeys")
                                     Maps.cfMapMapKeys),
                (("Base.Maps", "mapMapKeysWith"),
                 CustardNativeLambda ("Base.Maps", "cfMapMapKeysWith")
                                     Maps.cfMapMapKeysWith),
                (("Base.Maps", "mapMapKeysMonotonic"),
                 CustardNativeLambda ("Base.Maps", "cfMapMapKeysMonotonic")
                                     Maps.cfMapMapKeysMonotonic),
                -- Maps - Traversal - Fold
                (("Base.Maps", "mapFold"),
                 CustardNativeLambda ("Base.Maps", "cfMapFold")
                                     Maps.cfMapFold),
                (("Base.Maps", "mapFoldWithKey"),
                 CustardNativeLambda ("Base.Maps", "cfMapFoldWithKey")
                                     Maps.cfMapFoldWithKey),
                (("Base.Maps", "mapFoldrWithKey"),
                 CustardNativeLambda ("Base.Maps", "cfMapFoldrWithKey")
                                     Maps.cfMapFoldrWithKey),
                (("Base.Maps", "mapFoldlWithKey"),
                 CustardNativeLambda ("Base.Maps", "cfMapFoldlWithKey")
                                     Maps.cfMapFoldlWithKey),
                -- Maps - Conversion
                (("Base.Maps", "mapElems"),
                 CustardNativeLambda ("Base.Maps", "cfMapElems")
                                     Maps.cfMapElems),
                (("Base.Maps", "mapKeys"),
                 CustardNativeLambda ("Base.Maps", "cfMapKeys")
                                     Maps.cfMapKeys),
                (("Base.Maps", "mapKeysSet"),
                 CustardNativeLambda ("Base.Maps", "cfMapKeysSet")
                                     Maps.cfMapKeysSet),
                (("Base.Maps", "mapAssocs"),
                 CustardNativeLambda ("Base.Maps", "cfMapAssocs")
                                     Maps.cfMapAssocs),
                -- Maps - Conversion - Lists
                (("Base.Maps", "mapToList"),
                 CustardNativeLambda ("Base.Maps", "cfMapToList")
                                     Maps.cfMapToList),
                (("Base.Maps", "mapFromList"),
                 CustardNativeLambda ("Base.Maps", "cfMapFromList")
                                     Maps.cfMapFromList),
                (("Base.Maps", "mapFromListWith"),
                 CustardNativeLambda ("Base.Maps", "cfMapFromListWith")
                                     Maps.cfMapFromListWith),
                (("Base.Maps", "mapFromListWithKey"),
                 CustardNativeLambda ("Base.Maps", "cfMapFromListWithKey")
                                     Maps.cfMapFromListWithKey),
                -- Maps - Conversion - Ordered lists
                (("Base.Maps", "mapToAscList"),
                 CustardNativeLambda ("Base.Maps", "cfMapToAscList")
                                     Maps.cfMapToAscList),
                (("Base.Maps", "mapToDescList"),
                 CustardNativeLambda ("Base.Maps", "cfMapToDescList")
                                     Maps.cfMapToDescList),
                (("Base.Maps", "mapFromAscList"),
                 CustardNativeLambda ("Base.Maps", "cfMapFromAscList")
                                     Maps.cfMapFromAscList),
                (("Base.Maps", "mapFromAscListWith"),
                 CustardNativeLambda ("Base.Maps", "cfMapFromAscListWith")
                                     Maps.cfMapFromAscListWith),
                (("Base.Maps", "mapFromAscListWithKey"),
                 CustardNativeLambda ("Base.Maps", "cfMapFromAscListWithKey")
                                     Maps.cfMapFromAscListWithKey),
                (("Base.Maps", "mapFromDistinctAscList"),
                 CustardNativeLambda ("Base.Maps", "cfMapFromDistinctAscList")
                                     Maps.cfMapFromDistinctAscList),
                -- Maps - Filter
                (("Base.Maps", "mapFilter"),
                 CustardNativeLambda ("Base.Maps", "cfMapFilter")
                                     Maps.cfMapFilter),
                (("Base.Maps", "mapFilterWithKey"),
                 CustardNativeLambda ("Base.Maps", "cfMapFilterWithKey")
                                     Maps.cfMapFilterWithKey),
                (("Base.Maps", "mapPartition"),
                 CustardNativeLambda ("Base.Maps", "cfMapPartition")
                                     Maps.cfMapPartition),
                (("Base.Maps", "mapPartitionWithKey"),
                 CustardNativeLambda ("Base.Maps", "cfMapPartitionWithKey")
                                     Maps.cfMapPartitionWithKey),
                (("Base.Maps", "mapMaybe"),
                 CustardNativeLambda ("Base.Maps", "cfMapMaybe")
                                     Maps.cfMapMaybe),
                (("Base.Maps", "mapMaybeWithKey"),
                 CustardNativeLambda ("Base.Maps", "cfMapMaybeWithKey")
                                     Maps.cfMapMaybeWithKey),
                (("Base.Maps", "mapEither"),
                 CustardNativeLambda ("Base.Maps", "cfMapEither")
                                     Maps.cfMapEither),
                (("Base.Maps", "mapEitherWithKey"),
                 CustardNativeLambda ("Base.Maps", "cfMapEitherWithKey")
                                     Maps.cfMapEitherWithKey),
                (("Base.Maps", "mapSplit"),
                 CustardNativeLambda ("Base.Maps", "cfMapSplit")
                                     Maps.cfMapSplit),
                (("Base.Maps", "mapSplitLookup"),
                 CustardNativeLambda ("Base.Maps", "cfMapSplitLookup")
                                     Maps.cfMapSplitLookup),
                -- Maps - Submap
                (("Base.Maps", "mapIsSubmapOf"),
                 CustardNativeLambda ("Base.Maps", "cfMapIsSubmapOf")
                                     Maps.cfMapIsSubmapOf),
                (("Base.Maps", "mapIsSubmapOfBy"),
                 CustardNativeLambda ("Base.Maps", "cfMapIsSubmapOfBy")
                                     Maps.cfMapIsSubmapOfBy),
                (("Base.Maps", "mapIsProperSubmapOf"),
                 CustardNativeLambda ("Base.Maps", "cfMapIsProperSubmapOf")
                                     Maps.cfMapIsProperSubmapOf),
                (("Base.Maps", "mapIsProperSubmapOfBy"),
                 CustardNativeLambda ("Base.Maps", "cfMapIsProperSubmapOfBy")
                                     Maps.cfMapIsProperSubmapOfBy),
                -- Maps - Indexed
                (("Base.Maps", "mapLookupIndex"),
                 CustardNativeLambda ("Base.Maps", "cfMapLookupIndex")
                                     Maps.cfMapLookupIndex),
                (("Base.Maps", "mapFindIndex"),
                 CustardNativeLambda ("Base.Maps", "cfMapFindIndex")
                                     Maps.cfMapFindIndex),
                (("Base.Maps", "mapElemAt"),
                 CustardNativeLambda ("Base.Maps", "cfMapElemAt")
                                     Maps.cfMapElemAt),
                (("Base.Maps", "mapUpdateAt"),
                 CustardNativeLambda ("Base.Maps", "cfMapUpdateAt")
                                     Maps.cfMapUpdateAt),
                (("Base.Maps", "mapDeleteAt"),
                 CustardNativeLambda ("Base.Maps", "cfMapDeleteAt")
                                     Maps.cfMapDeleteAt),
                -- Maps - Min/Max
                (("Base.Maps", "mapFindMin"),
                 CustardNativeLambda ("Base.Maps", "cfMapFindMin")
                                     Maps.cfMapFindMin),
                (("Base.Maps", "mapFindMax"),
                 CustardNativeLambda ("Base.Maps", "cfMapFindMax")
                                     Maps.cfMapFindMax),
                (("Base.Maps", "mapDeleteMin"),
                 CustardNativeLambda ("Base.Maps", "cfMapDeleteMin")
                                     Maps.cfMapDeleteMin),
                (("Base.Maps", "mapDeleteMax"),
                 CustardNativeLambda ("Base.Maps", "cfMapDeleteMax")
                                     Maps.cfMapDeleteMax),
                (("Base.Maps", "mapDeleteFindMin"),
                 CustardNativeLambda ("Base.Maps", "cfMapDeleteFindMin")
                                     Maps.cfMapDeleteFindMin),
                (("Base.Maps", "mapDeleteFindMax"),
                 CustardNativeLambda ("Base.Maps", "cfMapDeleteFindMax")
                                     Maps.cfMapDeleteFindMax),
                (("Base.Maps", "mapUpdateMin"),
                 CustardNativeLambda ("Base.Maps", "cfMapUpdateMin")
                                     Maps.cfMapUpdateMin),
                (("Base.Maps", "mapUpdateMax"),
                 CustardNativeLambda ("Base.Maps", "cfMapUpdateMax")
                                     Maps.cfMapUpdateMax),
                (("Base.Maps", "mapUpdateMinWithKey"),
                 CustardNativeLambda ("Base.Maps", "cfMapUpdateMinWithKey")
                                     Maps.cfMapUpdateMinWithKey),
                (("Base.Maps", "mapUpdateMaxWithKey"),
                 CustardNativeLambda ("Base.Maps", "cfMapUpdateMaxWithKey")
                                     Maps.cfMapUpdateMaxWithKey),
                (("Base.Maps", "mapMinView"),
                 CustardNativeLambda ("Base.Maps", "cfMapMinView")
                                     Maps.cfMapMinView),
                (("Base.Maps", "mapMaxView"),
                 CustardNativeLambda ("Base.Maps", "cfMapMaxView")
                                     Maps.cfMapMaxView),
                (("Base.Maps", "mapMinViewWithKey"),
                 CustardNativeLambda ("Base.Maps", "cfMapMinViewWithKey")
                                     Maps.cfMapMinViewWithKey),
                (("Base.Maps", "mapMaxViewWithKey"),
                 CustardNativeLambda ("Base.Maps", "cfMapMaxViewWithKey")
                                     Maps.cfMapMaxViewWithKey)
               ]
