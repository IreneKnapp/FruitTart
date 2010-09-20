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
import qualified Data.ByteString as BS
import qualified Data.ByteString.UTF8 as UTF8
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
import qualified Network.FruitTart.Custard.Functions.Passwords as Passwords
import qualified Network.FruitTart.Custard.Functions.Captchas as Captchas
import qualified Network.FruitTart.Custard.Functions.Lists as Lists
import qualified Network.FruitTart.Custard.Functions.Strings as Strings
import qualified Network.FruitTart.Custard.Functions.Maps as Maps
import qualified Network.FruitTart.Custard.Functions.Data as Data
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
                         let resultString
                               = case result of
                                   CustardNull -> ""
                                   CustardString bytestring
                                     -> UTF8.toString bytestring
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
                         CustardString $ BS.concat [aString, bString])
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
        return (context, CustardString $ UTF8.fromString result)
      CustardCallBySymbolExpression subexpressions -> do
        if length subexpressions < 1
           then error $ "Invalid number of parameters to callBySYmbol()."
           else return ()
        (context, symbol) <- evalExpression context $ head subexpressions
        (moduleName, templateName)
          <- case symbol of
               CustardSymbol moduleName templateName
                 -> return (moduleName, templateName)
               _ -> error $ "First parameter to callBySymbol() does not "
                            ++ "evaluate to a symbol."
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
        return (context, CustardString $ UTF8.fromString result)
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
        return (context, CustardString $ UTF8.fromString $ concat results)
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
      fCatch (body context actualParameters)
             (\e -> error $ "In builtin " ++ moduleName ++ "." ++ properName
                            ++ ": " ++ (show (e :: SomeException)))
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
  = return $ SQLText $ UTF8.toString string
convertQueryValue (CustardInteger integer) = return $ SQLInteger integer
convertQueryValue (CustardString string)
  = return $ SQLText $ UTF8.toString string
convertQueryValue (CustardData bytestring) = return $ SQLBlob bytestring
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
                                      SQLText string
                                        -> CustardString $ UTF8.fromString string
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
                                                          $ UTF8.fromString
                                                             string
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
                 CustardNativeLambda ("Base", "Just")
                                     General.cfJust),
                (("Base", "LT"),
                 CustardOrdering LT),
                (("Base", "GT"),
                 CustardOrdering GT),
                (("Base", "EQ"),
                 CustardOrdering EQ),
                (("Base", "parameter"),
                 CustardNativeLambda ("Base", "parameter")
                                     General.cfParameter),
                (("Base", "isNothing"),
                 CustardNativeLambda ("Base", "isNothing")
                                     General.cfIsNothing),
                (("Base", "isJust"),
                 CustardNativeLambda ("Base", "isJust")
                                     General.cfIsJust),
                (("Base", "fromJust"),
                 CustardNativeLambda ("Base", "fromJust")
                                     General.cfFromJust),
                (("Base", "compareIntegers"),
                 CustardNativeLambda ("Base", "compareIntegers")
                                     General.cfCompareIntegers),
                (("Base", "showInteger"),
                 CustardNativeLambda ("Base", "showInteger")
                                     General.cfShowInteger),
                (("Base", "showBool"),
                 CustardNativeLambda ("Base", "showBool")
                                     General.cfShowBool),
                (("Base", "byteSizeToString"),
                 CustardNativeLambda ("Base", "byteSizeToString")
                                     General.cfByteSizeToString),
                (("Base", "timestampToString"),
                 CustardNativeLambda ("Base", "timestampToString")
                                     General.cfTimestampToString),
                (("Base", "getCurrentPage"),
                 CustardNativeLambda ("Base", "getCurrentPage")
                                     General.cfGetCurrentPage),
                (("Base", "getController"),
                 CustardNativeLambda ("Base", "getController")
                                     General.cfGetController),
                
                -- Symbols
                (("Base.Symbols", "symbolName"),
                 CustardNativeLambda ("Base.Symbols", "symbolName")
                                     Symbols.cfSymbolName),
                (("Base.Symbols", "symbolModule"),
                 CustardNativeLambda ("Base.Symbols", "symbolModule")
                                     Symbols.cfSymbolModule),
                (("Base.Symbols", "makeSymbol"),
                 CustardNativeLambda ("Base.Symbols", "makeSymbol")
                                     Symbols.cfMakeSymbol),
                
                -- HTTP
                (("Base.HTTP", "log"),
                 CustardNativeLambda ("Base.HTTP", "log")
                                     HTTP.cfLog),
                (("Base.HTTP", "getRequestVariable"),
                 CustardNativeLambda ("Base.HTTP", "getRequestVariable")
                                     HTTP.cfGetRequestVariable),
                (("Base.HTTP", "getAllRequestVariables"),
                 CustardNativeLambda ("Base.HTTP", "getAllRequestVariables")
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
                 CustardNativeLambda ("Base.HTTP", "getRequestHeader")
                                     HTTP.cfGetRequestHeader),
                (("Base.HTTP", "cookieName"),
                 CustardNativeLambda ("Base.HTTP", "cookieName")
                                     HTTP.cfCookieName),
                (("Base.HTTP", "cookieValue"),
                 CustardNativeLambda ("Base.HTTP", "cookieValue")
                                     HTTP.cfCookieValue),
                (("Base.HTTP", "cookieVersion"),
                 CustardNativeLambda ("Base.HTTP", "cookieVersion")
                                     HTTP.cfCookieVersion),
                (("Base.HTTP", "cookiePath"),
                 CustardNativeLambda ("Base.HTTP", "cookiePath")
                                     HTTP.cfCookiePath),
                (("Base.HTTP", "cookieDomain"),
                 CustardNativeLambda ("Base.HTTP", "cookieDomain")
                                     HTTP.cfCookieDomain),
                (("Base.HTTP", "cookieMaxAge"),
                 CustardNativeLambda ("Base.HTTP", "cookieMaxAge")
                                     HTTP.cfCookieMaxAge),
                (("Base.HTTP", "cookieSecure"),
                 CustardNativeLambda ("Base.HTTP", "cookieSecure")
                                     HTTP.cfCookieSecure),
                (("Base.HTTP", "cookieComment"),
                 CustardNativeLambda ("Base.HTTP", "cookieComment")
                                     HTTP.cfCookieComment),
                (("Base.HTTP", "getCookie"),
                 CustardNativeLambda ("Base.HTTP", "getCookie")
                                     HTTP.cfGetCookie),
                (("Base.HTTP", "getAllCookies"),
                 CustardNativeLambda ("Base.HTTP", "getAllCookies")
                                     HTTP.cfGetAllCookies),
                (("Base.HTTP", "getCookieValue"),
                 CustardNativeLambda ("Base.HTTP", "getCookieValue")
                                     HTTP.cfGetCookieValue),
                (("Base.HTTP", "getDocumentRoot"),
                 CustardNativeLambda ("Base.HTTP", "getDocumentRoot")
                                     HTTP.cfGetDocumentRoot),
                (("Base.HTTP", "getGatewayInterface"),
                 CustardNativeLambda ("Base.HTTP", "getGatewayInterface")
                                     HTTP.cfGetGatewayInterface),
                (("Base.HTTP", "getPathInfo"),
                 CustardNativeLambda ("Base.HTTP", "getPathInfo")
                                     HTTP.cfGetPathInfo),
                (("Base.HTTP", "getPathTranslated"),
                 CustardNativeLambda ("Base.HTTP", "getPathTranslated")
                                     HTTP.cfGetPathTranslated),
                (("Base.HTTP", "getQueryString"),
                 CustardNativeLambda ("Base.HTTP", "getQueryString")
                                     HTTP.cfGetQueryString),
                (("Base.HTTP", "getRedirectStatus"),
                 CustardNativeLambda ("Base.HTTP", "getRedirectStatus")
                                     HTTP.cfGetRedirectStatus),
                (("Base.HTTP", "getRedirectURI"),
                 CustardNativeLambda ("Base.HTTP", "getRedirectURI")
                                     HTTP.cfGetRedirectURI),
                (("Base.HTTP", "getRemoteAddress"),
                 CustardNativeLambda ("Base.HTTP", "getRemoteAddress")
                                     HTTP.cfGetRemoteAddress),
                (("Base.HTTP", "getRemotePort"),
                 CustardNativeLambda ("Base.HTTP", "getRemotePort")
                                     HTTP.cfGetRemotePort),
                (("Base.HTTP", "getRemoteHost"),
                 CustardNativeLambda ("Base.HTTP", "getRemoteHost")
                                     HTTP.cfGetRemoteHost),
                (("Base.HTTP", "getRemoteIdent"),
                 CustardNativeLambda ("Base.HTTP", "getRemoteIdent")
                                     HTTP.cfGetRemoteIdent),
                (("Base.HTTP", "getRemoteUser"),
                 CustardNativeLambda ("Base.HTTP", "getRemoteUser")
                                     HTTP.cfGetRemoteUser),
                (("Base.HTTP", "getRequestMethod"),
                 CustardNativeLambda ("Base.HTTP", "getRequestMethod")
                                     HTTP.cfGetRequestMethod),
                (("Base.HTTP", "getRequestURI"),
                 CustardNativeLambda ("Base.HTTP", "getRequestURI")
                                     HTTP.cfGetRequestURI),
                (("Base.HTTP", "getScriptFilename"),
                 CustardNativeLambda ("Base.HTTP", "getScriptFilename")
                                     HTTP.cfGetScriptFilename),
                (("Base.HTTP", "getScriptName"),
                 CustardNativeLambda ("Base.HTTP", "getScriptName")
                                     HTTP.cfGetScriptName),
                (("Base.HTTP", "getServerAddress"),
                 CustardNativeLambda ("Base.HTTP", "getServerAddress")
                                     HTTP.cfGetServerAddress),
                (("Base.HTTP", "getServerName"),
                 CustardNativeLambda ("Base.HTTP", "getServerName")
                                     HTTP.cfGetServerName),
                (("Base.HTTP", "getServerPort"),
                 CustardNativeLambda ("Base.HTTP", "getServerPort")
                                     HTTP.cfGetServerPort),
                (("Base.HTTP", "getServerProtocol"),
                 CustardNativeLambda ("Base.HTTP", "getServerProtocol")
                                     HTTP.cfGetServerProtocol),
                (("Base.HTTP", "getServerSoftware"),
                 CustardNativeLambda ("Base.HTTP", "getServerSoftware")
                                     HTTP.cfGetServerSoftware),
                (("Base.HTTP", "getAuthenticationType"),
                 CustardNativeLambda ("Base.HTTP", "getAuthenticationType")
                                     HTTP.cfGetAuthenticationType),
                (("Base.HTTP", "getContentLength"),
                 CustardNativeLambda ("Base.HTTP", "getContentLength")
                                     HTTP.cfGetContentLength),
                (("Base.HTTP", "getContentType"),
                 CustardNativeLambda ("Base.HTTP", "getContentType")
                                     HTTP.cfGetContentType),
                (("Base.HTTP", "setResponseStatus"),
                 CustardNativeLambda ("Base.HTTP", "setResponseStatus")
                                     HTTP.cfSetResponseStatus),
                (("Base.HTTP", "getResponseStatus"),
                 CustardNativeLambda ("Base.HTTP", "getResponseStatus")
                                     HTTP.cfGetResponseStatus),
                (("Base.HTTP", "setResponseHeader"),
                 CustardNativeLambda ("Base.HTTP", "setResponseHeader")
                                     HTTP.cfSetResponseHeader),
                (("Base.HTTP", "unsetResponseHeader"),
                 CustardNativeLambda ("Base.HTTP", "unsetResponseHeader")
                                     HTTP.cfUnsetResponseHeader),
                (("Base.HTTP", "getResponseHeader"),
                 CustardNativeLambda ("Base.HTTP", "getResponseHeader")
                                     HTTP.cfGetResponseHeader),
                (("Base.HTTP", "setCookie"),
                 CustardNativeLambda ("Base.HTTP", "setCookie")
                                     HTTP.cfSetCookie),
                (("Base.HTTP", "unsetCookie"),
                 CustardNativeLambda ("Base.HTTP", "unsetCookie")
                                     HTTP.cfUnsetCookie),
                (("Base.HTTP", "makeSimpleCookie"),
                 CustardNativeLambda ("Base.HTTP", "makeSimpleCookie")
                                     HTTP.cfMakeSimpleCookie),
                (("Base.HTTP", "makeCookie"),
                 CustardNativeLambda ("Base.HTTP", "makeCookie")
                                     HTTP.cfMakeCookie),
                (("Base.HTTP", "permanentRedirect"),
                 CustardNativeLambda ("Base.HTTP", "permanentRedirect")
                                     HTTP.cfPermanentRedirect),
                (("Base.HTTP", "seeOtherRedirect"),
                 CustardNativeLambda ("Base.HTTP", "seeOtherRedirect")
                                     HTTP.cfSeeOtherRedirect),
                (("Base.HTTP", "sendResponseHeaders"),
                 CustardNativeLambda ("Base.HTTP", "sendResponseHeaders")
                                     HTTP.cfSendResponseHeaders),
                (("Base.HTTP", "responseHeadersSent"),
                 CustardNativeLambda ("Base.HTTP", "responseHeadersSent")
                                     HTTP.cfResponseHeadersSent),
                (("Base.HTTP", "put"),
                 CustardNativeLambda ("Base.HTTP", "put")
                                     HTTP.cfPut),
                (("Base.HTTP", "putString"),
                 CustardNativeLambda ("Base.HTTP", "putString")
                                     HTTP.cfPutString),
                (("Base.HTTP", "closeOutput"),
                 CustardNativeLambda ("Base.HTTP", "closeOutput")
                                     HTTP.cfCloseOutput),
                
                -- Forms
                (("Base.Forms", "formInput"),
                 CustardNativeLambda ("Base.Forms", "formInput")
                                     Forms.cfFormInput),
                
                -- Sessions
                (("Base.Sessions", "getSessionID"),
                 CustardNativeLambda ("Base.Sessions", "getSessionID")
                                     Sessions.cfGetSessionID),
                
                -- Passwords
                (("Base.Passwords", "hashPassword"),
                 CustardNativeLambda ("Base.Password", "hashPassword")
                                     Passwords.cfHashPassword),
                
                -- Captchas
                (("Base.Captchas", "generateCaptcha"),
                 CustardNativeLambda ("Base.Captchas", "generateCaptcha")
                                     Captchas.cfGenerateCaptcha),
                (("Base.Captchas", "lookupCaptcha"),
                 CustardNativeLambda ("Base.Captchas", "lookupCaptcha")
                                     Captchas.cfLookupCaptcha),
                (("Base.Captchas", "checkCaptcha"),
                 CustardNativeLambda ("Base.Captchas", "checkCaptcha")
                                     Captchas.cfCheckCaptcha),
                (("Base.Captchas", "expireOldCaptchas"),
                 CustardNativeLambda ("Base.Captchas", "expireOldCaptchas")
                                     Captchas.cfExpireOldCaptchas),
                
                -- Everything below here is based on a corresponding function
                -- in Haskell, which may or may not have an identical name.
                
                -- Lists
                -- Lists - Basic functions
                (("Base.Lists", "head"),
                 CustardNativeLambda ("Base.Lists", "head")
                                     Lists.cfHead),
                (("Base.Lists", "last"),
                 CustardNativeLambda ("Base.Lists", "last")
                                     Lists.cfLast),
                (("Base.Lists", "tail"),
                 CustardNativeLambda ("Base.Lists", "tail")
                                     Lists.cfTail),
                (("Base.Lists", "init"),
                 CustardNativeLambda ("Base.Lists", "init")
                                     Lists.cfInit),
                (("Base.Lists", "null"),
                 CustardNativeLambda ("Base.Lists", "null")
                                     Lists.cfNull),
                (("Base.Lists", "length"),
                 CustardNativeLambda ("Base.Lists", "length")
                                     Lists.cfLength),
                -- Lists - List transformations
                (("Base.Lists", "map"),
                 CustardNativeLambda ("Base.Lists", "map")
                                     Lists.cfMap),
                (("Base.Lists", "reverse"),
                 CustardNativeLambda ("Base.Lists", "reverse")
                                     Lists.cfReverse),
                (("Base.Lists", "intersperse"),
                 CustardNativeLambda ("Base.Lists", "intersperse")
                                     Lists.cfIntersperse),
                (("Base.Lists", "intercalate"),
                 CustardNativeLambda ("Base.Lists", "intercalate")
                                     Lists.cfIntercalate),
                (("Base.Lists", "transpose"),
                 CustardNativeLambda ("Base.Lists", "transpose")
                                     Lists.cfTranspose),
                (("Base.Lists", "subsequences"),
                 CustardNativeLambda ("Base.Lists", "subsequences")
                                     Lists.cfSubsequences),
                (("Base.Lists", "permutations"),
                 CustardNativeLambda ("Base.Lists", "permutations")
                                     Lists.cfPermutations),
                -- Lists - Reducing lists (folds)
                (("Base.Lists", "fold"),
                 CustardNativeLambda ("Base.Lists", "fold")
                                     Lists.cfFold),
                (("Base.Lists", "fold1"),
                 CustardNativeLambda ("Base.Lists", "fold1")
                                     Lists.cfFold1),
                -- Lists - Reducing lists (folds) - Special folds
                (("Base.Lists", "concat"),
                 CustardNativeLambda ("Base.Lists", "concat")
                                     Lists.cfConcat),
                (("Base.Lists", "concatMap"),
                 CustardNativeLambda ("Base.Lists", "concatMap")
                                     Lists.cfConcatMap),
                (("Base.Lists", "and"),
                 CustardNativeLambda ("Base.Lists", "and")
                                     Lists.cfAnd),
                (("Base.Lists", "or"),
                 CustardNativeLambda ("Base.Lists", "or")
                                     Lists.cfOr),
                (("Base.Lists", "any"),
                 CustardNativeLambda ("Base.Lists", "any")
                                     Lists.cfAny),
                (("Base.Lists", "all"),
                 CustardNativeLambda ("Base.Lists", "all")
                                     Lists.cfAll),
                (("Base.Lists", "sum"),
                 CustardNativeLambda ("Base.Lists", "sum")
                                     Lists.cfSum),
                (("Base.Lists", "product"),
                 CustardNativeLambda ("Base.Lists", "product")
                                     Lists.cfProduct),
                (("Base.Lists", "maximum"),
                 CustardNativeLambda ("Base.Lists", "maximum")
                                     Lists.cfMaximum),
                (("Base.Lists", "minimum"),
                 CustardNativeLambda ("Base.Lists", "minimum")
                                     Lists.cfMinimum),
                -- Lists - Building lists
                -- Lists - Building lists - Scans
                (("Base.Lists", "scanl"),
                 CustardNativeLambda ("Base.Lists", "scanl")
                                     Lists.cfScanl),
                (("Base.Lists", "scanl1"),
                 CustardNativeLambda ("Base.Lists", "scanl1")
                                     Lists.cfScanl1),
                (("Base.Lists", "scanr"),
                 CustardNativeLambda ("Base.Lists", "scanr")
                                     Lists.cfScanr),
                (("Base.Lists", "scanr1"),
                 CustardNativeLambda ("Base.Lists", "scanr1")
                                     Lists.cfScanr1),
                -- Lists - Building lists - Accumulating maps
                (("Base.Lists", "mapAccumL"),
                 CustardNativeLambda ("Base.Lists", "mapAccumL")
                                     Lists.cfMapAccumL),
                (("Base.Lists", "mapAccumR"),
                 CustardNativeLambda ("Base.Lists", "mapAccumR")
                                     Lists.cfMapAccumR),
                -- Lists - Building lists - Replicate
                (("Base.Lists", "replicate"),
                 CustardNativeLambda ("Base.Lists", "replicate")
                                     Lists.cfReplicate),
                -- Lists - Building lists - Unfolding
                (("Base.Lists", "unfoldr"),
                 CustardNativeLambda ("Base.Lists", "unfoldr")
                                     Lists.cfUnfoldr),
                -- Lists - Sublists
                -- Lists - Sublists - Extracting sublists
                (("Base.Lists", "take"),
                 CustardNativeLambda ("Base.Lists", "take")
                                     Lists.cfTake),
                (("Base.Lists", "drop"),
                 CustardNativeLambda ("Base.Lists", "drop")
                                     Lists.cfDrop),
                (("Base.Lists", "splitAt"),
                 CustardNativeLambda ("Base.Lists", "splitAt")
                                     Lists.cfSplitAt),
                (("Base.Lists", "takeWhile"),
                 CustardNativeLambda ("Base.Lists", "takeWhile")
                                     Lists.cfTakeWhile),
                (("Base.Lists", "dropWhile"),
                 CustardNativeLambda ("Base.Lists", "dropWhile")
                                     Lists.cfDropWhile),
                (("Base.Lists", "span"),
                 CustardNativeLambda ("Base.Lists", "span")
                                     Lists.cfSpan),
                (("Base.Lists", "break"),
                 CustardNativeLambda ("Base.Lists", "break")
                                     Lists.cfBreak),
                (("Base.Lists", "stripPrefix"),
                 CustardNativeLambda ("Base.Lists", "stripPrefix")
                                     Lists.cfStripPrefix),
                (("Base.Lists", "group"),
                 CustardNativeLambda ("Base.Lists", "group")
                                     Lists.cfGroup),
                (("Base.Lists", "inits"),
                 CustardNativeLambda ("Base.Lists", "inits")
                                     Lists.cfInits),
                (("Base.Lists", "tails"),
                 CustardNativeLambda ("Base.Lists", "tails")
                                     Lists.cfTails),
                -- Lists - Sublists - Predicates
                (("Base.Lists", "isPrefixOf"),
                 CustardNativeLambda ("Base.Lists", "isPrefixOf")
                                     Lists.cfIsPrefixOf),
                (("Base.Lists", "isSuffixOf"),
                 CustardNativeLambda ("Base.Lists", "isSuffixOf")
                                     Lists.cfIsSuffixOf),
                (("Base.Lists", "isInfixOf"),
                 CustardNativeLambda ("Base.Lists", "isInfixOf")
                                     Lists.cfIsInfixOf),
                -- Lists - Searching lists
                -- Lists - Searching lists - Searching by equality
                (("Base.Lists", "elem"),
                 CustardNativeLambda ("Base.Lists", "elem")
                                     Lists.cfElem),
                (("Base.Lists", "notElem"),
                 CustardNativeLambda ("Base.Lists", "notElem")
                                     Lists.cfNotElem),
                (("Base.Lists", "lookup"),
                 CustardNativeLambda ("Base.Lists", "lookup")
                                     Lists.cfLookup),
                -- Lists - Searching lists - Searching with a predicate
                (("Base.Lists", "find"),
                 CustardNativeLambda ("Base.Lists", "find")
                                     Lists.cfFind),
                (("Base.Lists", "filter"),
                 CustardNativeLambda ("Base.Lists", "filter")
                                     Lists.cfFilter),
                (("Base.Lists", "partition"),
                 CustardNativeLambda ("Base.Lists", "partition")
                                     Lists.cfPartition),
                -- Lists - Indexing lists
                (("Base.Lists", "nth"),
                 CustardNativeLambda ("Base.Lists", "nth")
                                     Lists.cfNth),
                (("Base.Lists", "elemIndex"),
                 CustardNativeLambda ("Base.Lists", "elemIndex")
                                     Lists.cfElemIndex),
                (("Base.Lists", "elemIndices"),
                 CustardNativeLambda ("Base.Lists", "elemIndices")
                                     Lists.cfElemIndices),
                (("Base.Lists", "findIndex"),
                 CustardNativeLambda ("Base.Lists", "findIndex")
                                     Lists.cfFindIndex),
                (("Base.Lists", "findIndices"),
                 CustardNativeLambda ("Base.Lists", "findIndices")
                                     Lists.cfFindIndices),
                -- Lists - Special lists
                -- Lists - "Set" operations
                (("Base.Lists", "nub"),
                 CustardNativeLambda ("Base.Lists", "nub")
                                     Lists.cfNub),
                (("Base.Lists", "delete"),
                 CustardNativeLambda ("Base.Lists", "delete")
                                     Lists.cfDelete),
                (("Base.Lists", "deleteFirsts"),
                 CustardNativeLambda ("Base.Lists", "deleteFirsts")
                                     Lists.cfDeleteFirsts),
                (("Base.Lists", "union"),
                 CustardNativeLambda ("Base.Lists", "union")
                                     Lists.cfUnion),
                (("Base.Lists", "intersect"),
                 CustardNativeLambda ("Base.Lists", "intersect")
                                     Lists.cfIntersect),
                -- Lists - Ordered lists
                (("Base.Lists", "sort"),
                 CustardNativeLambda ("Base.Lists", "sort")
                                     Lists.cfSort),
                (("Base.Lists", "insert"),
                 CustardNativeLambda ("Base.Lists", "insert")
                                     Lists.cfInsert),
                -- Lists - Generalized functions
                (("Base.Lists", "nubBy"),
                 CustardNativeLambda ("Base.Lists", "nubBy")
                                     Lists.cfNubBy),
                (("Base.Lists", "deleteBy"),
                 CustardNativeLambda ("Base.Lists", "deleteBy")
                                     Lists.cfDeleteBy),
                (("Base.Lists", "deleteFirstsBy"),
                 CustardNativeLambda ("Base.Lists", "deleteFirstsBy")
                                     Lists.cfDeleteFirstsBy),
                (("Base.Lists", "unionBy"),
                 CustardNativeLambda ("Base.Lists", "unionBy")
                                     Lists.cfUnionBy),
                (("Base.Lists", "intersectBy"),
                 CustardNativeLambda ("Base.Lists", "intersectBy")
                                     Lists.cfIntersectBy),
                (("Base.Lists", "groupBy"),
                 CustardNativeLambda ("Base.Lists", "groupBy")
                                     Lists.cfGroupBy),
                (("Base.Lists", "sortBy"),
                 CustardNativeLambda ("Base.Lists", "sortBy")
                                     Lists.cfSortBy),
                (("Base.Lists", "insertBy"),
                 CustardNativeLambda ("Base.Lists", "insertBy")
                                     Lists.cfInsertBy),
                (("Base.Lists", "maximumBy"),
                 CustardNativeLambda ("Base.Lists", "maximumBy")
                                     Lists.cfMaximumBy),
                (("Base.Lists", "minimumBy"),
                 CustardNativeLambda ("Base.Lists", "minimumBy")
                                     Lists.cfMinimumBy),
                
                -- Strings
                -- Strings - Basic functions
                (("Base.Strings", "stringHead"),
                 CustardNativeLambda ("Base.Strings", "stringHead")
                                     Strings.cfStringHead),
                (("Base.Strings", "stringLast"),
                 CustardNativeLambda ("Base.Strings", "stringLast")
                                     Strings.cfStringLast),
                (("Base.Strings", "stringTail"),
                 CustardNativeLambda ("Base.Strings", "stringTail")
                                     Strings.cfStringTail),
                (("Base.Strings", "stringInit"),
                 CustardNativeLambda ("Base.Strings", "stringInit")
                                     Strings.cfStringInit),
                (("Base.Strings", "stringNull"),
                 CustardNativeLambda ("Base.Strings", "stringNull")
                                     Strings.cfStringNull),
                (("Base.Strings", "stringLength"),
                 CustardNativeLambda ("Base.Strings", "stringLength")
                                     Strings.cfStringLength),
                -- Strings - String transformations
                (("Base.Strings", "stringMap"),
                 CustardNativeLambda ("Base.Strings", "stringMap")
                                     Strings.cfStringMap),
                (("Base.Strings", "stringReverse"),
                 CustardNativeLambda ("Base.Strings", "stringReverse")
                                     Strings.cfStringReverse),
                (("Base.Strings", "stringIntersperse"),
                 CustardNativeLambda ("Base.Strings", "stringIntersperse")
                                     Strings.cfStringIntersperse),
                (("Base.Strings", "stringIntercalate"),
                 CustardNativeLambda ("Base.Strings", "stringIntercalate")
                                     Strings.cfStringIntercalate),
                (("Base.Strings", "stringTranspose"),
                 CustardNativeLambda ("Base.Strings", "stringTranspose")
                                     Strings.cfStringTranspose),
                (("Base.Strings", "stringSubsequences"),
                 CustardNativeLambda ("Base.Strings", "stringSubsequences")
                                     Strings.cfStringSubsequences),
                (("Base.Strings", "stringPermutations"),
                 CustardNativeLambda ("Base.Strings", "stringPermutations")
                                     Strings.cfStringPermutations),
                -- Strings - Reducing strings (folds)
                (("Base.Strings", "stringFold"),
                 CustardNativeLambda ("Base.Strings", "stringFold")
                                     Strings.cfStringFold),
                (("Base.Strings", "stringFold1"),
                 CustardNativeLambda ("Base.Strings", "stringFold1")
                                     Strings.cfStringFold1),
                -- Strings - Reducing strings (folds) - Special folds
                (("Base.Strings", "stringConcat"),
                 CustardNativeLambda ("Base.Strings", "stringConcat")
                                     Strings.cfStringConcat),
                (("Base.Strings", "stringConcatMap"),
                 CustardNativeLambda ("Base.Strings", "stringConcatMap")
                                     Strings.cfStringConcatMap),
                (("Base.Strings", "stringAny"),
                 CustardNativeLambda ("Base.Strings", "stringAny")
                                     Strings.cfStringAny),
                (("Base.Strings", "stringAll"),
                 CustardNativeLambda ("Base.Strings", "stringAll")
                                     Strings.cfStringAll),
                -- Strings - Building strings
                -- Strings - Building strings - Scans
                (("Base.Strings", "stringScanl"),
                 CustardNativeLambda ("Base.Strings", "stringScanl")
                                     Strings.cfStringScanl),
                (("Base.Strings", "stringScanl1"),
                 CustardNativeLambda ("Base.Strings", "stringScanl1")
                                     Strings.cfStringScanl1),
                (("Base.Strings", "stringScanr"),
                 CustardNativeLambda ("Base.Strings", "stringScanr")
                                     Strings.cfStringScanr),
                (("Base.Strings", "stringScanr1"),
                 CustardNativeLambda ("Base.Strings", "stringScanr1")
                                     Strings.cfStringScanr1),
                -- Strings - Building strings - Accumulating maps
                (("Base.Strings", "stringMapAccumL"),
                 CustardNativeLambda ("Base.Strings", "stringMapAccumL")
                                     Strings.cfStringMapAccumL),
                (("Base.Strings", "stringMapAccumR"),
                 CustardNativeLambda ("Base.Strings", "stringMapAccumR")
                                     Strings.cfStringMapAccumR),
                -- Strings - Building strings - Replicate
                (("Base.Strings", "stringReplicate"),
                 CustardNativeLambda ("Base.Strings", "stringReplicate")
                                     Strings.cfStringReplicate),
                -- Strings - Building strings - Unfolding
                (("Base.Strings", "stringUnfoldr"),
                 CustardNativeLambda ("Base.Strings", "stringUnfoldr")
                                     Strings.cfStringUnfoldr),
                -- Strings - Substrings
                -- Strings - Substrings - Extracting sublists
                (("Base.Strings", "stringTake"),
                 CustardNativeLambda ("Base.Strings", "stringTake")
                                     Strings.cfStringTake),
                (("Base.Strings", "stringDrop"),
                 CustardNativeLambda ("Base.Strings", "stringDrop")
                                     Strings.cfStringDrop),
                (("Base.Strings", "stringSplitAt"),
                 CustardNativeLambda ("Base.Strings", "stringSplitAt")
                                     Strings.cfStringSplitAt),
                (("Base.Strings", "stringTakeWhile"),
                 CustardNativeLambda ("Base.Strings", "stringTakeWhile")
                                     Strings.cfStringTakeWhile),
                (("Base.Strings", "stringDropWhile"),
                 CustardNativeLambda ("Base.Strings", "stringDropWhile")
                                     Strings.cfStringDropWhile),
                (("Base.Strings", "stringSpan"),
                 CustardNativeLambda ("Base.Strings", "stringSpan")
                                     Strings.cfStringSpan),
                (("Base.Strings", "stringBreak"),
                 CustardNativeLambda ("Base.Strings", "stringBreak")
                                     Strings.cfStringBreak),
                (("Base.Strings", "stringStripPrefix"),
                 CustardNativeLambda ("Base.Strings", "stringStripPrefix")
                                     Strings.cfStringStripPrefix),
                (("Base.Strings", "stringGroup"),
                 CustardNativeLambda ("Base.Strings", "stringGroup")
                                     Strings.cfStringGroup),
                (("Base.Strings", "stringInits"),
                 CustardNativeLambda ("Base.Strings", "stringInits")
                                     Strings.cfStringInits),
                (("Base.Strings", "stringTails"),
                 CustardNativeLambda ("Base.Strings", "stringTails")
                                     Strings.cfStringTails),
                -- Strings - Substrings - Predicates
                (("Base.Strings", "stringIsPrefixOf"),
                 CustardNativeLambda ("Base.Strings", "stringIsPrefixOf")
                                     Strings.cfStringIsPrefixOf),
                (("Base.Strings", "stringIsSuffixOf"),
                 CustardNativeLambda ("Base.Strings", "stringIsSuffixOf")
                                     Strings.cfStringIsSuffixOf),
                (("Base.Strings", "stringIsInfixOf"),
                 CustardNativeLambda ("Base.Strings", "stringIsInfixOf")
                                     Strings.cfStringIsInfixOf),
                -- Strings - Searching strings
                -- Strings - Searching strings - Searching by equality
                (("Base.Strings", "stringElem"),
                 CustardNativeLambda ("Base.Strings", "stringElem")
                                     Strings.cfStringElem),
                (("Base.Strings", "stringNotElem"),
                 CustardNativeLambda ("Base.Strings", "stringNotElem")
                                     Strings.cfStringNotElem),
                -- Strings - Searching strings - Searching with a predicate
                (("Base.Strings", "stringFind"),
                 CustardNativeLambda ("Base.Strings", "stringFind")
                                     Strings.cfStringFind),
                (("Base.Strings", "stringFilter"),
                 CustardNativeLambda ("Base.Strings", "stringFilter")
                                     Strings.cfStringFilter),
                (("Base.Strings", "stringPartition"),
                 CustardNativeLambda ("Base.Strings", "stringPartition")
                                     Strings.cfStringPartition),
                -- Strings - Indexing strings
                (("Base.Strings", "stringNth"),
                 CustardNativeLambda ("Base.Strings", "stringNth")
                                     Strings.cfStringNth),
                (("Base.Strings", "stringElemIndex"),
                 CustardNativeLambda ("Base.Strings", "stringElemIndex")
                                     Strings.cfStringElemIndex),
                (("Base.Strings", "stringElemIndices"),
                 CustardNativeLambda ("Base.Strings", "stringElemIndices")
                                     Strings.cfStringElemIndices),
                (("Base.Strings", "stringFindIndex"),
                 CustardNativeLambda ("Base.Strings", "stringFindIndex")
                                     Strings.cfStringFindIndex),
                (("Base.Strings", "stringFindIndices"),
                 CustardNativeLambda ("Base.Strings", "stringFindIndices")
                                     Strings.cfStringFindIndices),
                -- Strings - Text operations
                (("Base.Strings", "stringLines"),
                 CustardNativeLambda ("Base.Strings", "stringLines")
                                     Strings.cfStringLines),
                (("Base.Strings", "stringWords"),
                 CustardNativeLambda ("Base.Strings", "stringWords")
                                     Strings.cfStringWords),
                (("Base.Strings", "stringUnlines"),
                 CustardNativeLambda ("Base.Strings", "stringUnlines")
                                     Strings.cfStringUnlines),
                (("Base.Strings", "stringUnwords"),
                 CustardNativeLambda ("Base.Strings", "stringUnwords")
                                     Strings.cfStringUnwords),
                
                -- Maps
                -- Maps - Query
                (("Base.Maps", "mapNull"),
                 CustardNativeLambda ("Base.Maps", "mapNull")
                                     Maps.cfMapNull),
                (("Base.Maps", "mapSize"),
                 CustardNativeLambda ("Base.Maps", "mapSize")
                                     Maps.cfMapSize),
                (("Base.Maps", "mapMember"),
                 CustardNativeLambda ("Base.Maps", "mapMember")
                                     Maps.cfMapMember),
                (("Base.Maps", "mapNotMember"),
                 CustardNativeLambda ("Base.Maps", "mapNotMember")
                                     Maps.cfMapNotMember),
                (("Base.Maps", "mapLookup"),
                 CustardNativeLambda ("Base.Maps", "mapLookup")
                                     Maps.cfMapLookup),
                (("Base.Maps", "mapFindWithDefault"),
                 CustardNativeLambda ("Base.Maps", "mapFindWithDefault")
                                     Maps.cfMapFindWithDefault),
                -- Maps - Construction
                (("Base.Maps", "makeEmptyMap"),
                 CustardNativeLambda ("Base.Maps", "makeEmptyMap")
                                     Maps.cfMakeEmptyMap),
                (("Base.Maps", "makeSingletonMap"),
                 CustardNativeLambda ("Base.Maps", "makeSingletonMap")
                                     Maps.cfMakeSingletonMap),
                -- Maps - Construction - Insertion
                (("Base.Maps", "mapInsert"),
                 CustardNativeLambda ("Base.Maps", "mapInsert")
                                     Maps.cfMapInsert),
                (("Base.Maps", "mapInsertWith"),
                 CustardNativeLambda ("Base.Maps", "mapInsertWith")
                                     Maps.cfMapInsertWith),
                (("Base.Maps", "mapInsertWithKey"),
                 CustardNativeLambda ("Base.Maps", "mapInsertWithKey")
                                     Maps.cfMapInsertWithKey),
                (("Base.Maps", "mapInsertLookupWithKey"),
                 CustardNativeLambda ("Base.Maps", "mapInsertLookupWithKey")
                                     Maps.cfMapInsertLookupWithKey),
                -- Maps - Construction - Delete/Update
                (("Base.Maps", "mapDelete"),
                 CustardNativeLambda ("Base.Maps", "mapDelete")
                                     Maps.cfMapDelete),
                (("Base.Maps", "mapAdjust"),
                 CustardNativeLambda ("Base.Maps", "mapAdjust")
                                     Maps.cfMapAdjust),
                (("Base.Maps", "mapAdjustWithKey"),
                 CustardNativeLambda ("Base.Maps", "mapAdjustWithKey")
                                     Maps.cfMapAdjustWithKey),
                (("Base.Maps", "mapUpdate"),
                 CustardNativeLambda ("Base.Maps", "mapUpdate")
                                     Maps.cfMapUpdate),
                (("Base.Maps", "mapUpdateWithKey"),
                 CustardNativeLambda ("Base.Maps", "mapUpdateWithKey")
                                     Maps.cfMapUpdateWithKey),
                (("Base.Maps", "mapUpdateLookupWithKey"),
                 CustardNativeLambda ("Base.Maps", "mapUpdateLookupWithKey")
                                     Maps.cfMapUpdateLookupWithKey),
                (("Base.Maps", "mapAlter"),
                 CustardNativeLambda ("Base.Maps", "mapAlter")
                                     Maps.cfMapAlter),
                -- Maps - Combine
                -- Maps - Combine - Union
                (("Base.Maps", "mapUnion"),
                 CustardNativeLambda ("Base.Maps", "mapUnion")
                                     Maps.cfMapUnion),
                (("Base.Maps", "mapUnionWith"),
                 CustardNativeLambda ("Base.Maps", "mapUnionWith")
                                     Maps.cfMapUnionWith),
                (("Base.Maps", "mapUnionWithKey"),
                 CustardNativeLambda ("Base.Maps", "mapUnionWithKey")
                                     Maps.cfMapUnionWithKey),
                (("Base.Maps", "mapUnions"),
                 CustardNativeLambda ("Base.Maps", "mapUnions")
                                     Maps.cfMapUnions),
                (("Base.Maps", "mapUnionsWith"),
                 CustardNativeLambda ("Base.Maps", "mapUnionsWith")
                                     Maps.cfMapUnionsWith),
                -- Maps - Combine - Difference
                (("Base.Maps", "mapDifference"),
                 CustardNativeLambda ("Base.Maps", "mapDifference")
                                     Maps.cfMapDifference),
                (("Base.Maps", "mapDifferenceWith"),
                 CustardNativeLambda ("Base.Maps", "mapDifferenceWith")
                                     Maps.cfMapDifferenceWith),
                (("Base.Maps", "mapDifferenceWithKey"),
                 CustardNativeLambda ("Base.Maps", "mapDifferenceWithKey")
                                     Maps.cfMapDifferenceWithKey),
                -- Maps - Combine - Intersection
                (("Base.Maps", "mapIntersection"),
                 CustardNativeLambda ("Base.Maps", "mapIntersection")
                                     Maps.cfMapIntersection),
                (("Base.Maps", "mapIntersectionWith"),
                 CustardNativeLambda ("Base.Maps", "mapIntersectionWith")
                                     Maps.cfMapIntersectionWith),
                (("Base.Maps", "mapIntersectionWithKey"),
                 CustardNativeLambda ("Base.Maps", "mapIntersectionWithKey")
                                     Maps.cfMapIntersectionWithKey),
                -- Maps - Traversal
                -- Maps - Traversal - Map
                (("Base.Maps", "mapMap"),
                 CustardNativeLambda ("Base.Maps", "mapMap")
                                     Maps.cfMapMap),
                (("Base.Maps", "mapMapWithKey"),
                 CustardNativeLambda ("Base.Maps", "mapMapWithKey")
                                     Maps.cfMapMapWithKey),
                (("Base.Maps", "mapMapAccum"),
                 CustardNativeLambda ("Base.Maps", "mapMapAccum")
                                     Maps.cfMapMapAccum),
                (("Base.Maps", "mapMapAccumWithKey"),
                 CustardNativeLambda ("Base.Maps", "mapMapAccumWithKey")
                                     Maps.cfMapMapAccumWithKey),
                (("Base.Maps", "mapMapAccumRWithKey"),
                 CustardNativeLambda ("Base.Maps", "mapMapAccumRWithKey")
                                     Maps.cfMapMapAccumRWithKey),
                (("Base.Maps", "mapMapKeys"),
                 CustardNativeLambda ("Base.Maps", "mapMapKeys")
                                     Maps.cfMapMapKeys),
                (("Base.Maps", "mapMapKeysWith"),
                 CustardNativeLambda ("Base.Maps", "mapMapKeysWith")
                                     Maps.cfMapMapKeysWith),
                (("Base.Maps", "mapMapKeysMonotonic"),
                 CustardNativeLambda ("Base.Maps", "mapMapKeysMonotonic")
                                     Maps.cfMapMapKeysMonotonic),
                -- Maps - Traversal - Fold
                (("Base.Maps", "mapFold"),
                 CustardNativeLambda ("Base.Maps", "mapFold")
                                     Maps.cfMapFold),
                (("Base.Maps", "mapFoldrWithKey"),
                 CustardNativeLambda ("Base.Maps", "mapFoldrWithKey")
                                     Maps.cfMapFoldrWithKey),
                (("Base.Maps", "mapFoldlWithKey"),
                 CustardNativeLambda ("Base.Maps", "mapFoldlWithKey")
                                     Maps.cfMapFoldlWithKey),
                -- Maps - Conversion
                (("Base.Maps", "mapElems"),
                 CustardNativeLambda ("Base.Maps", "mapElems")
                                     Maps.cfMapElems),
                (("Base.Maps", "mapKeys"),
                 CustardNativeLambda ("Base.Maps", "mapKeys")
                                     Maps.cfMapKeys),
                (("Base.Maps", "mapKeysSet"),
                 CustardNativeLambda ("Base.Maps", "mapKeysSet")
                                     Maps.cfMapKeysSet),
                (("Base.Maps", "mapAssocs"),
                 CustardNativeLambda ("Base.Maps", "mapAssocs")
                                     Maps.cfMapAssocs),
                -- Maps - Conversion - Lists
                (("Base.Maps", "mapToList"),
                 CustardNativeLambda ("Base.Maps", "mapToList")
                                     Maps.cfMapToList),
                (("Base.Maps", "mapFromList"),
                 CustardNativeLambda ("Base.Maps", "mapFromList")
                                     Maps.cfMapFromList),
                (("Base.Maps", "mapFromListWith"),
                 CustardNativeLambda ("Base.Maps", "mapFromListWith")
                                     Maps.cfMapFromListWith),
                (("Base.Maps", "mapFromListWithKey"),
                 CustardNativeLambda ("Base.Maps", "mapFromListWithKey")
                                     Maps.cfMapFromListWithKey),
                -- Maps - Conversion - Ordered lists
                (("Base.Maps", "mapToAscList"),
                 CustardNativeLambda ("Base.Maps", "mapToAscList")
                                     Maps.cfMapToAscList),
                (("Base.Maps", "mapToDescList"),
                 CustardNativeLambda ("Base.Maps", "mapToDescList")
                                     Maps.cfMapToDescList),
                (("Base.Maps", "mapFromAscList"),
                 CustardNativeLambda ("Base.Maps", "mapFromAscList")
                                     Maps.cfMapFromAscList),
                (("Base.Maps", "mapFromAscListWith"),
                 CustardNativeLambda ("Base.Maps", "mapFromAscListWith")
                                     Maps.cfMapFromAscListWith),
                (("Base.Maps", "mapFromAscListWithKey"),
                 CustardNativeLambda ("Base.Maps", "mapFromAscListWithKey")
                                     Maps.cfMapFromAscListWithKey),
                (("Base.Maps", "mapFromDistinctAscList"),
                 CustardNativeLambda ("Base.Maps", "mapFromDistinctAscList")
                                     Maps.cfMapFromDistinctAscList),
                -- Maps - Filter
                (("Base.Maps", "mapFilter"),
                 CustardNativeLambda ("Base.Maps", "mapFilter")
                                     Maps.cfMapFilter),
                (("Base.Maps", "mapFilterWithKey"),
                 CustardNativeLambda ("Base.Maps", "mapFilterWithKey")
                                     Maps.cfMapFilterWithKey),
                (("Base.Maps", "mapPartition"),
                 CustardNativeLambda ("Base.Maps", "mapPartition")
                                     Maps.cfMapPartition),
                (("Base.Maps", "mapPartitionWithKey"),
                 CustardNativeLambda ("Base.Maps", "mapPartitionWithKey")
                                     Maps.cfMapPartitionWithKey),
                (("Base.Maps", "mapMaybe"),
                 CustardNativeLambda ("Base.Maps", "mapMaybe")
                                     Maps.cfMapMaybe),
                (("Base.Maps", "mapMaybeWithKey"),
                 CustardNativeLambda ("Base.Maps", "mapMaybeWithKey")
                                     Maps.cfMapMaybeWithKey),
                (("Base.Maps", "mapEither"),
                 CustardNativeLambda ("Base.Maps", "mapEither")
                                     Maps.cfMapEither),
                (("Base.Maps", "mapEitherWithKey"),
                 CustardNativeLambda ("Base.Maps", "mapEitherWithKey")
                                     Maps.cfMapEitherWithKey),
                (("Base.Maps", "mapSplit"),
                 CustardNativeLambda ("Base.Maps", "mapSplit")
                                     Maps.cfMapSplit),
                (("Base.Maps", "mapSplitLookup"),
                 CustardNativeLambda ("Base.Maps", "mapSplitLookup")
                                     Maps.cfMapSplitLookup),
                -- Maps - Submap
                (("Base.Maps", "mapIsSubmapOf"),
                 CustardNativeLambda ("Base.Maps", "mapIsSubmapOf")
                                     Maps.cfMapIsSubmapOf),
                (("Base.Maps", "mapIsSubmapOfBy"),
                 CustardNativeLambda ("Base.Maps", "mapIsSubmapOfBy")
                                     Maps.cfMapIsSubmapOfBy),
                (("Base.Maps", "mapIsProperSubmapOf"),
                 CustardNativeLambda ("Base.Maps", "mapIsProperSubmapOf")
                                     Maps.cfMapIsProperSubmapOf),
                (("Base.Maps", "mapIsProperSubmapOfBy"),
                 CustardNativeLambda ("Base.Maps", "mapIsProperSubmapOfBy")
                                     Maps.cfMapIsProperSubmapOfBy),
                -- Maps - Indexed
                (("Base.Maps", "mapLookupIndex"),
                 CustardNativeLambda ("Base.Maps", "mapLookupIndex")
                                     Maps.cfMapLookupIndex),
                (("Base.Maps", "mapFindIndex"),
                 CustardNativeLambda ("Base.Maps", "mapFindIndex")
                                     Maps.cfMapFindIndex),
                (("Base.Maps", "mapElemAt"),
                 CustardNativeLambda ("Base.Maps", "mapElemAt")
                                     Maps.cfMapElemAt),
                (("Base.Maps", "mapUpdateAt"),
                 CustardNativeLambda ("Base.Maps", "mapUpdateAt")
                                     Maps.cfMapUpdateAt),
                (("Base.Maps", "mapDeleteAt"),
                 CustardNativeLambda ("Base.Maps", "mapDeleteAt")
                                     Maps.cfMapDeleteAt),
                -- Maps - Min/Max
                (("Base.Maps", "mapFindMin"),
                 CustardNativeLambda ("Base.Maps", "mapFindMin")
                                     Maps.cfMapFindMin),
                (("Base.Maps", "mapFindMax"),
                 CustardNativeLambda ("Base.Maps", "mapFindMax")
                                     Maps.cfMapFindMax),
                (("Base.Maps", "mapDeleteMin"),
                 CustardNativeLambda ("Base.Maps", "mapDeleteMin")
                                     Maps.cfMapDeleteMin),
                (("Base.Maps", "mapDeleteMax"),
                 CustardNativeLambda ("Base.Maps", "mapDeleteMax")
                                     Maps.cfMapDeleteMax),
                (("Base.Maps", "mapDeleteFindMin"),
                 CustardNativeLambda ("Base.Maps", "mapDeleteFindMin")
                                     Maps.cfMapDeleteFindMin),
                (("Base.Maps", "mapDeleteFindMax"),
                 CustardNativeLambda ("Base.Maps", "mapDeleteFindMax")
                                     Maps.cfMapDeleteFindMax),
                (("Base.Maps", "mapUpdateMin"),
                 CustardNativeLambda ("Base.Maps", "mapUpdateMin")
                                     Maps.cfMapUpdateMin),
                (("Base.Maps", "mapUpdateMax"),
                 CustardNativeLambda ("Base.Maps", "mapUpdateMax")
                                     Maps.cfMapUpdateMax),
                (("Base.Maps", "mapUpdateMinWithKey"),
                 CustardNativeLambda ("Base.Maps", "mapUpdateMinWithKey")
                                     Maps.cfMapUpdateMinWithKey),
                (("Base.Maps", "mapUpdateMaxWithKey"),
                 CustardNativeLambda ("Base.Maps", "mapUpdateMaxWithKey")
                                     Maps.cfMapUpdateMaxWithKey),
                (("Base.Maps", "mapMinView"),
                 CustardNativeLambda ("Base.Maps", "mapMinView")
                                     Maps.cfMapMinView),
                (("Base.Maps", "mapMaxView"),
                 CustardNativeLambda ("Base.Maps", "mapMaxView")
                                     Maps.cfMapMaxView),
                (("Base.Maps", "mapMinViewWithKey"),
                 CustardNativeLambda ("Base.Maps", "mapMinViewWithKey")
                                     Maps.cfMapMinViewWithKey),
                (("Base.Maps", "mapMaxViewWithKey"),
                 CustardNativeLambda ("Base.Maps", "mapMaxViewWithKey")
                                     Maps.cfMapMaxViewWithKey),
                
                -- Data
                -- Data - Introducing and eliminating data
                (("Base.Data", "makeEmptyData"),
                 CustardNativeLambda ("Base.Data", "makeEmptyData")
                                     Data.cfMakeEmptyData),
                (("Base.Data", "makeSingletonData"),
                 CustardNativeLambda ("Base.Data", "makeSingletonData")
                                     Data.cfMakeSingletonData),
                (("Base.Data", "dataPack"),
                 CustardNativeLambda ("Base.Data", "dataPack")
                                     Data.cfDataPack),
                (("Base.Data", "dataUnpack"),
                 CustardNativeLambda ("Base.Data", "dataUnpack")
                                     Data.cfDataUnpack),
                -- Data - Basic interface
                (("Base.Data", "dataCons"),
                 CustardNativeLambda ("Base.Data", "dataCons")
                                     Data.cfDataCons),
                (("Base.Data", "dataSnoc"),
                 CustardNativeLambda ("Base.Data", "dataSnoc")
                                     Data.cfDataSnoc),
                (("Base.Data", "dataAppend"),
                 CustardNativeLambda ("Base.Data", "dataAppend")
                                     Data.cfDataAppend),
                (("Base.Data", "dataHead"),
                 CustardNativeLambda ("Base.Data", "dataHead")
                                     Data.cfDataHead),
                (("Base.Data", "dataUncons"),
                 CustardNativeLambda ("Base.Data", "dataUncons")
                                     Data.cfDataUncons),
                (("Base.Data", "dataLast"),
                 CustardNativeLambda ("Base.Data", "dataLast")
                                     Data.cfDataLast),
                (("Base.Data", "dataTail"),
                 CustardNativeLambda ("Base.Data", "dataTail")
                                     Data.cfDataTail),
                (("Base.Data", "dataInit"),
                 CustardNativeLambda ("Base.Data", "dataInit")
                                     Data.cfDataInit),
                (("Base.Data", "dataNull"),
                 CustardNativeLambda ("Base.Data", "dataNull")
                                     Data.cfDataNull),
                (("Base.Data", "dataLength"),
                 CustardNativeLambda ("Base.Data", "dataLength")
                                     Data.cfDataLength),
                -- Data - Transforming data
                (("Base.Data", "dataMap"),
                 CustardNativeLambda ("Base.Data", "dataMap")
                                     Data.cfDataMap),
                (("Base.Data", "dataReverse"),
                 CustardNativeLambda ("Base.Data", "dataReverse")
                                     Data.cfDataReverse),
                (("Base.Data", "dataIntersperse"),
                 CustardNativeLambda ("Base.Data", "dataIntersperse")
                                     Data.cfDataIntersperse),
                (("Base.Data", "dataIntercalate"),
                 CustardNativeLambda ("Base.Data", "dataIntercalate")
                                     Data.cfDataIntercalate),
                (("Base.Data", "dataTranspose"),
                 CustardNativeLambda ("Base.Data", "dataTranspose")
                                     Data.cfDataTranspose),
                -- Data - Reducing data (folds)
                (("Base.Data", "dataFold"),
                 CustardNativeLambda ("Base.Data", "dataFold")
                                     Data.cfDataFold),
                (("Base.Data", "dataFold1"),
                 CustardNativeLambda ("Base.Data", "dataFold1")
                                     Data.cfDataFold1),
                -- Data - Reducing data (folds) - Special folds
                (("Base.Data", "dataConcat"),
                 CustardNativeLambda ("Base.Data", "dataConcat")
                                     Data.cfDataConcat),
                (("Base.Data", "dataConcatMap"),
                 CustardNativeLambda ("Base.Data", "dataConcatMap")
                                     Data.cfDataConcatMap),
                (("Base.Data", "dataAny"),
                 CustardNativeLambda ("Base.Data", "dataAny")
                                     Data.cfDataAny),
                (("Base.Data", "dataAll"),
                 CustardNativeLambda ("Base.Data", "dataAll")
                                     Data.cfDataAll),
                (("Base.Data", "dataMaximum"),
                 CustardNativeLambda ("Base.Data", "dataMaximum")
                                     Data.cfDataMaximum),
                (("Base.Data", "dataMinimum"),
                 CustardNativeLambda ("Base.Data", "dataMinimum")
                                     Data.cfDataMinimum),
                -- Data - Building data
                -- Data - Building data - Scans
                (("Base.Data", "dataScanl"),
                 CustardNativeLambda ("Base.Data", "dataScanl")
                                     Data.cfDataScanl),
                (("Base.Data", "dataScanl1"),
                 CustardNativeLambda ("Base.Data", "dataScanl1")
                                     Data.cfDataScanl1),
                (("Base.Data", "dataScanr"),
                 CustardNativeLambda ("Base.Data", "dataScanr")
                                     Data.cfDataScanr),
                (("Base.Data", "dataScanr1"),
                 CustardNativeLambda ("Base.Data", "dataScanr1")
                                     Data.cfDataScanr1),
                -- Data - Building data - Accumulating maps
                (("Base.Data", "dataMapAccumL"),
                 CustardNativeLambda ("Base.Data", "dataMapAccumL")
                                     Data.cfDataMapAccumL),
                (("Base.Data", "dataMapAccumR"),
                 CustardNativeLambda ("Base.Data", "dataMapAccumR")
                                     Data.cfDataMapAccumR),
                -- Data - Building data - Generating and unfolding data
                (("Base.Data", "dataReplicate"),
                 CustardNativeLambda ("Base.Data", "dataReplicate")
                                     Data.cfDataReplicate),
                (("Base.Data", "dataUnfoldr"),
                 CustardNativeLambda ("Base.Data", "dataUnfoldr")
                                     Data.cfDataUnfoldr),
                (("Base.Data", "dataUnfoldrN"),
                 CustardNativeLambda ("Base.Data", "dataUnfoldrN")
                                     Data.cfDataUnfoldrN),
                -- Data - Substrings
                -- Data - Substrings - Breaking strings
                (("Base.Data", "dataTake"),
                 CustardNativeLambda ("Base.Data", "dataTake")
                                     Data.cfDataTake),
                (("Base.Data", "dataDrop"),
                 CustardNativeLambda ("Base.Data", "dataDrop")
                                     Data.cfDataDrop),
                (("Base.Data", "dataSplitAt"),
                 CustardNativeLambda ("Base.Data", "dataSplitAt")
                                     Data.cfDataSplitAt),
                (("Base.Data", "dataTakeWhile"),
                 CustardNativeLambda ("Base.Data", "dataTakeWhile")
                                     Data.cfDataTakeWhile),
                (("Base.Data", "dataDropWhile"),
                 CustardNativeLambda ("Base.Data", "dataDropWhile")
                                     Data.cfDataDropWhile),
                (("Base.Data", "dataSpan"),
                 CustardNativeLambda ("Base.Data", "dataSpan")
                                     Data.cfDataSpan),
                (("Base.Data", "dataSpanEnd"),
                 CustardNativeLambda ("Base.Data", "dataSpanEnd")
                                     Data.cfDataSpanEnd),
                (("Base.Data", "dataBreak"),
                 CustardNativeLambda ("Base.Data", "dataBreak")
                                     Data.cfDataBreak),
                (("Base.Data", "dataBreakEnd"),
                 CustardNativeLambda ("Base.Data", "dataBreakEnd")
                                     Data.cfDataBreakEnd),
                (("Base.Data", "dataBreakByte"),
                 CustardNativeLambda ("Base.Data", "dataBreakByte")
                                     Data.cfDataBreakByte),
                (("Base.Data", "dataGroup"),
                 CustardNativeLambda ("Base.Data", "dataGroup")
                                     Data.cfDataGroup),
                (("Base.Data", "dataGroupBy"),
                 CustardNativeLambda ("Base.Data", "dataGroupBy")
                                     Data.cfDataGroupBy),
                (("Base.Data", "dataInits"),
                 CustardNativeLambda ("Base.Data", "dataInits")
                                     Data.cfDataInits),
                (("Base.Data", "dataTails"),
                 CustardNativeLambda ("Base.Data", "dataTails")
                                     Data.cfDataTails),
                -- Data - Substrings - Breaking into many substrings
                (("Base.Data", "dataSplit"),
                 CustardNativeLambda ("Base.Data", "dataSplit")
                                     Data.cfDataSplit),
                (("Base.Data", "dataSplitWith"),
                 CustardNativeLambda ("Base.Data", "dataSplitWith")
                                     Data.cfDataSplitWith),
                -- Data - Predicates
                (("Base.Data", "dataIsPrefixOf"),
                 CustardNativeLambda ("Base.Data", "dataIsPrefixOf")
                                     Data.cfDataIsPrefixOf),
                (("Base.Data", "dataIsSuffixOf"),
                 CustardNativeLambda ("Base.Data", "dataIsSuffixOf")
                                     Data.cfDataIsSuffixOf),
                (("Base.Data", "dataIsInfixOf"),
                 CustardNativeLambda ("Base.Data", "dataIsInfixOf")
                                     Data.cfDataIsInfixOf),
                -- Data - Predicates - Search for arbitrary substrings
                (("Base.Data", "dataBreakSubstring"),
                 CustardNativeLambda ("Base.Data", "dataBreakSubstring")
                                     Data.cfDataBreakSubstring),
                (("Base.Data", "dataFindSubstring"),
                 CustardNativeLambda ("Base.Data", "dataFindSubstring")
                                     Data.cfDataFindSubstring),
                (("Base.Data", "dataFindSubstrings"),
                 CustardNativeLambda ("Base.Data", "dataFindSubstrings")
                                     Data.cfDataFindSubstrings),
                -- Data - Searching data
                -- Data - Searching data - Searching by equality
                (("Base.Data", "dataElem"),
                 CustardNativeLambda ("Base.Data", "dataElem")
                                     Data.cfDataElem),
                (("Base.Data", "dataNotElem"),
                 CustardNativeLambda ("Base.Data", "dataNotElem")
                                     Data.cfDataNotElem),
                -- Data - Searching data - Searching with a predicate
                (("Base.Data", "dataFind"),
                 CustardNativeLambda ("Base.Data", "dataFind")
                                     Data.cfDataFind),
                (("Base.Data", "dataFilter"),
                 CustardNativeLambda ("Base.Data", "dataFilter")
                                     Data.cfDataFilter),
                (("Base.Data", "dataPartition"),
                 CustardNativeLambda ("Base.Data", "dataPartition")
                                     Data.cfDataPartition),
                -- Data - Indexing data
                (("Base.Data", "dataIndex"),
                 CustardNativeLambda ("Base.Data", "dataIndex")
                                     Data.cfDataIndex),
                (("Base.Data", "dataElemIndex"),
                 CustardNativeLambda ("Base.Data", "dataElemIndex")
                                     Data.cfDataElemIndex),
                (("Base.Data", "dataElemIndices"),
                 CustardNativeLambda ("Base.Data", "dataElemIndices")
                                     Data.cfDataElemIndices),
                (("Base.Data", "dataElemIndexEnd"),
                 CustardNativeLambda ("Base.Data", "dataElemIndexEnd")
                                     Data.cfDataElemIndexEnd),
                (("Base.Data", "dataFindIndex"),
                 CustardNativeLambda ("Base.Data", "dataFindIndex")
                                     Data.cfDataFindIndex),
                (("Base.Data", "dataFindIndices"),
                 CustardNativeLambda ("Base.Data", "dataFindIndices")
                                     Data.cfDataFindIndices),
                (("Base.Data", "dataCount"),
                 CustardNativeLambda ("Base.Data", "dataCount")
                                     Data.cfDataCount),
                -- Data - Zipping and unzipping data
                (("Base.Data", "dataZip"),
                 CustardNativeLambda ("Base.Data", "dataZip")
                                     Data.cfDataZip),
                (("Base.Data", "dataZipWith"),
                 CustardNativeLambda ("Base.Data", "dataZipWith")
                                     Data.cfDataZipWith),
                (("Base.Data", "dataUnzip"),
                 CustardNativeLambda ("Base.Data", "dataUnzip")
                                     Data.cfDataUnzip),
                -- Data - Ordered data
                (("Base.Data", "dataSort"),
                 CustardNativeLambda ("Base.Data", "dataSort")
                                     Data.cfDataSort),
                -- Data - Copying data
                (("Base.Data", "dataCopy"),
                 CustardNativeLambda ("Base.Data", "dataCopy")
                                     Data.cfDataCopy),
                -- Data - UTF8
                (("Base.Data", "dataUTF8Decode"),
                 CustardNativeLambda ("Base.Data", "dataUTF8Decode")
                                     Data.cfDataUTF8Decode),
                (("Base.Data", "dataUTF8Uncons"),
                 CustardNativeLambda ("Base.Data", "dataUTF8Uncons")
                                     Data.cfDataUTF8Uncons),
                (("Base.Data", "dataUTF8SplitAt"),
                 CustardNativeLambda ("Base.Data", "dataUTF8SplitAt")
                                     Data.cfDataUTF8SplitAt),
                (("Base.Data", "dataUTF8Take"),
                 CustardNativeLambda ("Base.Data", "dataUTF8Take")
                                     Data.cfDataUTF8Take),
                (("Base.Data", "dataUTF8Drop"),
                 CustardNativeLambda ("Base.Data", "dataUTF8Drop")
                                     Data.cfDataUTF8Drop),
                (("Base.Data", "dataUTF8Span"),
                 CustardNativeLambda ("Base.Data", "dataUTF8Span")
                                     Data.cfDataUTF8Span),
                (("Base.Data", "dataUTF8Break"),
                 CustardNativeLambda ("Base.Data", "dataUTF8Break")
                                     Data.cfDataUTF8Break),
                (("Base.Data", "dataUTF8FromString"),
                 CustardNativeLambda ("Base.Data", "dataUTF8FromString")
                                     Data.cfDataUTF8FromString),
                (("Base.Data", "dataUTF8ToString"),
                 CustardNativeLambda ("Base.Data", "dataUTF8ToString")
                                     Data.cfDataUTF8ToString),
                (("Base.Data", "dataUTF8Fold"),
                 CustardNativeLambda ("Base.Data", "dataUTF8Fold")
                                     Data.cfDataUTF8Fold),
                (("Base.Data", "dataUTF8Length"),
                 CustardNativeLambda ("Base.Data", "dataUTF8Length")
                                     Data.cfDataUTF8Length),
                (("Base.Data", "dataUTF8Lines"),
                 CustardNativeLambda ("Base.Data", "dataUTF8Lines")
                                     Data.cfDataUTF8Lines)
               ]
