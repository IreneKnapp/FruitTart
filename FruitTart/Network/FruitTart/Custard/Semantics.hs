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

import Network.FruitTart.Custard.Syntax
import qualified Network.FruitTart.Custard.Functions.Forms as Forms
import qualified Network.FruitTart.Custard.Functions.Captchas as Captchas
import qualified Network.FruitTart.Custard.Functions.Lists as Lists
import qualified Network.FruitTart.Custard.Functions.Strings as Strings
import Network.FruitTart.Common
import Network.FruitTart.Types
import Network.FruitTart.Util


getTemplateWithParameters :: String
                          -> String
                          -> CustardContextType
                          -> Map String String
                          -> [AnyCustardValue]
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
                  custardContextBindings = Map.empty
                }
  getTemplateWithContext context


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
            "content" -> return body
            "expression" ->
                fCatch (do
                         FruitTartState { database = database } <- get
                         expression <- liftIO $ readExpression database
                                                               moduleName
                                                               body
                         (result, newBindings)
                           <- evalExpression context expression
                         let context'= context {
                                         custardContextBindings = newBindings
                                       }
                         resultString <- valueToStringAllowingNull result
                         return (context', accumulator ++ resultString))
                       (\e -> error $ "While processing template "
                                    ++ moduleName ++ "."
                                    ++ templateName ++ ", item " ++ (show index)
                                    ++ ": " ++ (show (e :: SomeException)))
            _ -> error $ "Unknown template item type " ++ kind ++ ".")
        (context, "")
        (zip items [1..])
        >>= return . snd


eval :: String -> String -> FruitTart AnyCustardValue
eval moduleName body = do
  let context = CustardContext {
                  custardContextType = TemplateContext,
                  custardContextFormInputMap = Map.empty,
                  custardContextParameters = [],
                  custardContextBindings = Map.empty
                }
  FruitTartState { database = database } <- get
  expression <- liftIO $ readExpression database moduleName body
  (result, _) <- evalExpression context expression
  return result


valueToStringAllowingNull :: AnyCustardValue -> FruitTart String
valueToStringAllowingNull (SomeCustardValue (CustardString string))
  = return string
valueToStringAllowingNull (SomeCustardValue TemplateNull) = return ""
valueToStringAllowingNull value
  = error $ "Value is not a String or Null."


evalExpression :: CustardContext
               -> CustardExpression
               -> FruitTart (CustardContext, AnyCustardValue)
evalExpression context expression = do
    case expression of
      CustardStringLiteral value -> return (context, SomeCustardValue value)
      CustardIntegerLiteral value -> return (context, SomeCustardValue value)
      CustardExpressionList subexpressions -> do
        (context, values)
          <- foldM (\(context, values) subexpression -> do
                      (context, value) <- evalExpression context subexpression
                      return (context, values ++ [value]))
                   (context, [])
                   subexpressions
        return (context, SomeCustardValue $ CustardList values)
      CustardOperationConcatenate aExpression bExpression -> do
        (context, aValue) <- evalExpression context aExpression >>= return . fst
        (context, bValue) <- evalExpression context bExpression >>= return . fst
        case (aValue, bValue) of
          (CustardString aString, CustardString bString)
              -> return (context,
                         SomeCustardValue $ CustardString $ aString ++ bString)
          _ -> error "Cannot concatenate non-Strings."
      CustardOperationEquals aExpression bExpression -> do
        (context, aValue) <- evalExpression context aExpression >>= return . fst
        (context, bValue) <- evalExpression context bExpression >>= return . fst
        result <- custardEqual aValue bValue
        return (context, result)
      CustardOperationNotEquals aExpression bExpression -> do
        (context, aValue) <- evalExpression context aExpression >>= return . fst
        (context, bValue) <- evalExpression context bExpression >>= return . fst
        result <- custardEqual aValue bValue >>= return . fst
        result <- custardNegate result
        return (context, result)
      CustardOperationNot expression -> do
        (context, value) <- evalExpression context expression >>= return . fst
        result <- custardNegate value
        return (context, result)
      CustardOperationAnd aExpression bExpression -> do
        (context, aValue) <- evalExpression context aExpression >>= return . fst
        (context, bValue) <- evalExpression context bExpression >>= return . fst
        result <- custardAnd aValue bValue
        return (context, result)
      CustardOperationOr aExpression bExpression -> do
        (context, aValue) <- evalExpression context aExpression >>= return . fst
        (context, bValue) <- evalExpression context bExpression >>= return . fst
        result <- custardOr aValue bValue
        return (context, result)
      CustardOperationGreaterEquals aExpression bExpression -> do
        (context, aValue) <- evalExpression context aExpression >>= return . fst
        (context, bValue) <- evalExpression context bExpression >>= return . fst
        result <- custardCompare aValue bValue
        return (context, SomeCustardValue $ CustardBool $ case result of
                                                            EQ -> True
                                                            GT -> True
                                                            LT -> False)
      CustardOperationGreater aExpression bExpression -> do
        (context, aValue) <- evalExpression context aExpression >>= return . fst
        (context, bValue) <- evalExpression context bExpression >>= return . fst
        result <- custardCompare aValue bValue
        return (context, SomeCustardValue $ CustardBool $ case result of
                                                            EQ -> False
                                                            GT -> True
                                                            LT -> False)
      CustardOperationLessEquals aExpression bExpression -> do
        (context, aValue) <- evalExpression context aExpression >>= return . fst
        (context, bValue) <- evalExpression context bExpression >>= return . fst
        result <- custardCompare aValue bValue
        return (context, SomeCustardValue $ CustardBool $ case result of
                                                            EQ -> True
                                                            GT -> False
                                                            LT -> True)
      CustardOperationLess aExpression bExpression -> do
        (context, aValue) <- evalExpression context aExpression >>= return . fst
        (context, bValue) <- evalExpression context bExpression >>= return . fst
        result <- custardCompare aValue bValue
        return (context, SomeCustardValue $ CustardBool $ case result of
                                                            EQ -> False
                                                            GT -> False
                                                            LT -> True)
      CustardOperationAdd aExpression bExpression -> do
        (context, aValue) <- evalExpression context aExpression >>= return . fst
        (context, bValue) <- evalExpression context bExpression >>= return . fst
        result <- custardArithmetic aValue bValue (+)
        return (context, result)
      CustardOperationSubtract aExpression bExpression -> do
        (context, aValue) <- evalExpression context aExpression >>= return . fst
        (context, bValue) <- evalExpression context bExpression >>= return . fst
        result <- custardArithmetic aValue bValue (-)
        return (context, result)
      CustardOperationMultiply aExpression bExpression -> do
        (context, aValue) <- evalExpression context aExpression >>= return . fst
        (context, bValue) <- evalExpression context bExpression >>= return . fst
        result <- custardArithmetic aValue bValue (*)
        return (context, result)
      CustardOperationDivide aExpression bExpression -> do
        (context, aValue) <- evalExpression context aExpression >>= return . fst
        (context, bValue) <- evalExpression context bExpression >>= return . fst
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
                             >>= valueToBoolean . fst
        if result
          then evalExpression context ifTrue
          else evalExpression context ifFalse
      CustardCaseExpression subexpressions -> do
        let n = length subexpressions
        if not ((n > 1) && (odd n))
          then error $ "Invalid number of parameters to case()."
          else return ()
        (context, mainKey) <- (evalExpression context $ head subexpressions)
                              >>= return . fst
        let case' items = do
              case items of
                [] -> error $ "No match in case() for " ++ (show mainKey) ++ "."
                (key:(value:rest)) -> do
                  case key of
                    CustardVariable (_, "otherwise") ->
                      evalExpression context value
                    _ -> do
                      key <- evalExpression context key >>= return . fst
                      if mainKey == key
                        then evalExpression context value
                        else case' rest
        case' $ tail subexpressions
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
        let subcontext = context { templateContextParameters = subparameters }
        result <- getTemplateWithContext moduleName templateName subcontext
        return (context, SomeCustardValue $ CustardString result)
      CustardIterateExpression subexpressions -> do
        if length subexpressions < 2
           then error $ "Invalid number of parameters to iterate()."
           else return ()
        (moduleName, templateName)
            <- case head subexpressions of
                 CustardVariable result -> return result
                 _ -> error $ "First parameter to iterate() is not a symbol."
        (context, rows) <- valueToListOfMaps
                           $ evalExpression context $ subexpressions !! 1
        (context, subparameters)
          <- foldM (\(context, values) subexpression -> do
                      (context, value) <- evalExpression context subexpression
                      return (context, values ++ [value]))
                   (context, [])
                   $ drop 2 subexpressions
        results <- mapM (\row -> do
                           let CustardContext { custardContextBindings
                                                  = oldBindings } = context
                               newBindings = Map.union row oldBindings
                               subcontext = context { custardContextBindings
                                                        = newBindings }
                           getTemplateWithContext moduleName
                                                  templateName
                                                  subcontext)
                        rows
        return (context, SomeCustardValue $ CustardString $ concat results)
      CustardQueryExpression subexpressions -> do
        if length subexpressions < 1
           then error $ "Invalid number of parameters to query()."
           else return ()
        parameters <- (mapM (evalExpression context) $ drop 1 subexpressions)
                      >>= return . map fst
        (moduleName, queryName)
            <- case head subexpressions of
                 CustardVariable result -> return result
                 _ -> error $ "First parameter to query() is not a symbol."
        mapM (\parameter -> case parameter of
               SomeCustardValue (CustardBool True)
                 -> return $ SQLInteger 1
               SomeCustardValue (CustardBool False)
                 -> return $ SQLInteger 0
               SomeCustardValue (CustardMaybe Nothing)
                 -> return $ SQLNull
               SomeCustardValue (CustardMaybe (Just (CustardInteger integer)))
                 -> return $ SQLInteger integer
               SomeCustardValue (CustardMaybe (Just (CustardString string)))
                 -> return $ SQLText string
               SomeCustardValue (CustardInteger integer)
                 -> return $ SQLInteger integer
               SomeCustardValue (CustardString string)
                 -> return $ SQLText string
               _ -> error "Invalid type for query parameter.")
             parameters
             >>= namedQuery moduleName queryName
        return (context, SomeCustardValue $ CustardNull)
      CustardBoundExpression subexpressions -> do
        if length subexpressions /= 1
          then error $ "Invalid number of parameters to bound()."
          else return ()
        name
            <- case head subexpressions of
                 CustardVariable result -> return result
                 _ -> error $ "Parameter to bound() is not a symbol."
        let CustardContext { custardContextBindings = bindings } = context
        return (context,
                case Map.lookup name bindings of
                  Nothing -> SomeCustardValue $ CustardBool False
                  Just _ -> SomeCustardValue $ CustardBool True)
      CustardFunctionCall functionExpression actualParameterExpressions -> do
        function <- evalExpression context functionExpression
                    >>= return . fst
        (context, actualParameters)
          <- foldM (\(context, values) subexpression -> do
                      (context, value) <- evalExpression context subexpression
                      return (context, values ++ [value]))
                   (context, [])
                   actualParameterExpressions
        applyFunctionGivenContextAndValue context function actualParameters
      CustardLambdaExpression formalParameters body -> do
        let CustardContext { custardContextBindings = bindings } = context
        return (context, CustardLambda formalParameters bindings body)
      CustardVariable name@(packageName, properName) -> do
        let CustardContext { custardContextBindings = bindings } = context
        case Map.lookup name bindings of
          Nothing -> case Map.lookup ("Base", properName) bindings of
                       Nothing -> do
                         maybeValue <- getTopLevelBinding name
                         case maybeValue of
                           Just value -> do
                             let CustardContext { custardContextBindings
                                                    = oldBindings } = context
                                 newBindings = Map.union
                                                (Map.fromList [(name, value)])
                                                oldBindings
                                 context' = context { custardContextBindings
                                                        = newBindings }
                             return (context', value)
                           Nothing -> error $ "Undefined variable "
                                              ++ packageName ++ "."
                                              ++ properName ++ "."
                       Just value -> return (context, value)
          Just value -> return (context, value)
      CustardBindExpression subexpressions -> do
        if length subexpressions /= 2
          then error $ "Invalid number of parameters to bind()."
          else return ()
        name
            <- case head subexpressions of
                 CustardVariable result -> return result
                 _ -> error $ "Parameter to bind() is not a symbol."
        value <- (evalExpression context $ head $ drop 1 subexpressions)
                 >>= return . fst
        let CustardContext { custardContextBindings = oldBindings } = context
            newBindings = Map.union (Map.fromList [(name, value)])
                                    oldBindings
            context' = context { custardContextBindings = newBindings }
        return (context', CustardNull)
      CustardBindMapExpression subexpressions -> do
        if length subexpressions /= 1
          then error $ "Invalid number of parameters to bindmap()."
          else return ()
        value <- (evalExpression context $ head subexpressions)
                 >>= return . fst
        passedMap <- valueToMap value
        let CustardContext { custardContextBindings = oldBindings } = context
            newBindings = Map.union passedMap oldBindings
            context' = context { custardContextBindings = newBindings }
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
        bindQuery1 context (moduleName, queryName) inputs
      CustardBindQueryNExpression subexpressions -> do
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
        bindQueryN context (moduleName, queryName) inputs
      CustardSequence expressionA expressionB -> do
        (context, _) <- evalExpression context expressionA
        evalExpression context expressionB


custardEqual :: AnyCustardValue
              -> AnyCustardValue
              -> FruitTart AnyCustardValue
custardEqual (CustardBool a) (CustardBool b)
    = return $ SomeCustardValue $ CustardBool $ a == b
custardEqual (TemplateInteger a) (TemplateInteger b)
    = return $ SomeCustardValue $ CustardBool $ a == b
custardEqual (CustardCharacter a) (CustardCharacter b)
    = return $ SomeCustardValue $ CustardBool $ a == b
custardEqual (CustardString a) (CustardString b)
    = return $ SomeCustardValue $ CustardBool $ a == b
custardEqual (TemplateOrdering a) (TemplateOrdering b)
    = return $ SomeCustardValue $ CustardBool $ a == b
custardEqual _ _
    = error $ "Values in comparison are not the same type or "
            ++ "are not Booleans, Integers, Characters, Strings, or Orderings."


custardNegate :: AnyCustardValue
               -> FruitTart AnyCustardValue
custardNegate (CustardBool value)
    = return $ SomeCustardValue $ CustardBool $ not value
custardNegate _ = error "Value in logical operation is not a Boolean."


custardAnd :: AnyCustardValue
            -> AnyCustardValue
            -> FruitTart AnyCustardValue
custardAnd (CustardBool a) (CustardBool b)
    = return $ SomeCustardValue $ CustardBool $ a && b
custardAnd _ _ = error "Values in logical operation are not both Booleans."


custardOr :: AnyCustardValue
           -> AnyCustardValue
           -> FruitTart AnyCustardValue
custardOr (CustardBool a) (CustardBool b)
    = return $ SomeCustardValue $ CustardBool $ a || b
custardOr _ _ = error "Values in logical operation are not both Booleans."


custardCompare :: AnyCustardValue -> AnyCustardValue -> FruitTart Ordering
custardCompare (TemplateInteger a) (TemplateInteger b) = return $ compare a b
custardCompare (CustardCharacter a) (CustardCharacter b) = return $ compare a b
custardCompare _ _ = error $  "Values in comparison are not the same type or "
                     ++ " are not Integers or Characters."


custardArithmetic :: AnyCustardValue
                   -> AnyCustardValue
                   -> (Int64 -> Int64 -> Int64)
                   -> AnyCustardValue
custardArithmetic (TemplateInteger a) (TemplateInteger b) operation
    = return $ SomeCustardValue $ TemplateInteger $ operation a b
custardArithmetic _ _ _ = error "Values in arithmetic are not both Integers."


applyFunctionGivenName :: CustardContextType
                       -> Map String String
                       -> String
                       -> String
                       -> [AnyCustardValue]
                       -> FruitTart (CustardContext, AnyCustardValue)
applyFunctionGivenName contextType
                       formInputMap
                       moduleName
                       functionName
                       parameters = do
  let context = CustardContext {
                  custardContextType = contextType,
                  custardContextFormInputMap = formInputMap,
                  custardContextParameters = parameters,
                  custardContextBindings = Map.empty
                }
  applyFunctionGivenContextAndValue context function parameters


applyFunctionGivenContextAndValue :: CustardContext
                                  -> AnyCustardValue
                                  -> [AnyCustardValue]
                                  -> FruitTart (CustardContext, AnyCustardValue)
applyFunctionGivenContextAndValue context function actualParameters = do
  case function of
    TemplateLambda formalParameters capturedBindings body -> do
      if length formalParameters /= length actualParameters
        then error $ "Expected " ++ (show $ length formalParameters)
                   ++ " parameters, but got " ++ (show $ length actualParameters)
                   ++ "."
        else return ()
      let newBindings = Map.fromList $ zip (map (\(TemplateParameter key) -> key)
                                                formalParameters)
                                           actualParameters
          subbindings = Map.union newBindings capturedBindings
      letBindings subbindings (evalExpression context body >>= return . fst)
    TemplateNativeLambda body -> do
      body context actualParameters
    _ -> do
      error $ "Call to something not a function."


getTopLevelBinding :: (String, String) -> FruitTart (Maybe AnyCustardValue)
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
                                 -> TemplateParameter (moduleName, parameterName))
                               parameterItems
          return $ Just $ TemplateLambda parameters Map.empty compiledBody
        _ -> return Nothing


bindQuery1 :: CustardContext
           -> (String, String)
           -> [AnyCustardValue]
           -> FruitTart CustardContext
bindQuery1 context (moduleName, queryName) inputs = do
  error "Not yet implemented."
  -- TODO


bindQueryN :: CustardContext
           -> (String, String)
           -> [AnyCustardValue]
           -> FruitTart CustardContext
bindQueryN context (moduleName, queryName) inputs = do
  error "Not yet implemented."
  -- TODO


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
                           SomeCustardValue
                           $ TemplateList
                           $ map (\row -> SomeCustardValue
                                          $ TemplateMap
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


getValueNamesAndTypes :: Int64 -> FruitTart [(String, TemplateValueType)]
getValueNamesAndTypes queryID = do
  rows <- query "SELECT name, type FROM query_results WHERE query = ? ORDER BY item"
                [SQLInteger queryID]
  return $ map (\[SQLText name, SQLText typeName] ->
                 (name,
                  case typeName of
                    "boolean" -> TBool
                    "integer" -> TInt
                    "string" -> TString
                    "maybeInteger" -> TMaybeInt
                    "maybeString" -> TMaybeString
                    _ -> TInt))
               rows


namedQuery :: String -> String -> [SQLData]
           -> FruitTart [Map (String, String) AnyCustardValue]
namedQuery moduleName queryName queryValues = do
  rows <- query ("SELECT id, body FROM queries "
                ++ " WHERE module = ? AND name = ?")
                [SQLText moduleName, SQLText queryName]
  case rows of
    [[SQLInteger queryID, SQLText queryText]] -> do
      valueNamesAndTypes <- getValueNamesAndTypes queryID
      rows <- query queryText queryValues
      return $ map (\row -> convertRowToBindings moduleName
                                                 valueNamesAndTypes
                                                 row)
                   rows
    _ -> error $ "Query " ++ moduleName ++ "." ++ queryName ++ " not found."


convertRowToBindings :: String -> [(String, TemplateValueType)] -> [SQLData]
                     -> Map (String, String) AnyCustardValue
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
                         TBool -> case value of
                                    SQLInteger integer -> SomeCustardValue
                                                          $ TemplateBool
                                                          $ integer /= 0
                                    _ -> error "Value from query not an integer."
                         TInt -> case value of
                                   SQLInteger integer -> SomeCustardValue
                                                         $ TemplateInteger
                                                         $ integer
                                   _ -> error "Value from query not an integer."
                         TString -> case value of
                                      SQLText string -> SomeCustardValue
                                                        $ CustardString string
                                      _ -> error "Value from query not a string."
                         TMaybeInt -> case value of
                                        SQLNull -> SomeCustardValue
                                                   $ TemplateMaybe Nothing
                                        SQLInteger integer -> SomeCustardValue
                                                              $ TemplateMaybe
                                                              $ Just
                                                              $ TemplateInteger
                                                              $ integer
                                        _ -> error
                                             "Value from query not an integer or null."
                         TMaybeString -> case value of
                                        SQLNull -> SomeCustardValue
                                                   $ TemplateMaybe Nothing
                                        SQLText string -> SomeCustardValue
                                                          $ TemplateMaybe
                                                          $ Just
                                                          $ CustardString
                                                          $ string
                                        _ -> error
                                             "Value from query not a string or null."))
                 $ zip valueNamesAndTypes row
 -}


builtinBindings :: Map (String, String) AnyCustardValue
builtinBindings = Map.fromList
               [(("Base", "parameters"),
                 SomeCustardValue $ TemplateList []),
                (("Base", "Null"),
                 SomeCustardValue $ TemplateNull),
                (("Base", "True"),
                 SomeCustardValue $ CustardBool True),
                (("Base", "False"),
                 SomeCustardValue $ CustardBool False),
                (("Base", "Nothing"),
                 SomeCustardValue $ TemplateMaybe Nothing),
                (("Base", "Just"),
                 SomeCustardValue $ TemplateNativeLambda cfJust),
                (("Base", "LT"),
                 SomeCustardValue $ TemplateOrdering LT),
                (("Base", "GT"),
                 SomeCustardValue $ TemplateOrdering GT),
                (("Base", "EQ"),
                 SomeCustardValue $ TemplateOrdering EQ),
                (("Base", "parameter"),
                 SomeCustardValue $ TemplateNativeLambda cfParameter),
                (("Base", "isNothing"),
                 SomeCustardValue $ TemplateNativeLambda cfIsNothing),
                (("Base", "isJust"),
                 SomeCustardValue $ TemplateNativeLambda cfIsJust),
                (("Base", "fromJust"),
                 SomeCustardValue $ TemplateNativeLambda cfFromJust),
                (("Base", "stringWordCount"),
                 SomeCustardValue $ TemplateNativeLambda cfStringWordCount),
                (("Base", "compareIntegers"),
                 SomeCustardValue $ TemplateNativeLambda cfCompareIntegers),
                (("Base", "showInteger"),
                 SomeCustardValue $ TemplateNativeLambda cfShowInteger),
                (("Base", "showBool"),
                 SomeCustardValue $ TemplateNativeLambda cfShowBool),
                (("Base", "byteSizeToString"),
                 SomeCustardValue $ TemplateNativeLambda cfByteSizeToString),
                (("Base", "timestampToString"),
                 SomeCustardValue $ TemplateNativeLambda cfTimestampToString),
                (("Base", "escapeAttribute"),
                 SomeCustardValue $ TemplateNativeLambda cfEscapeAttribute),
                (("Base", "escapeHTML"),
                 SomeCustardValue $ TemplateNativeLambda cfEscapeHTML),
                (("Base", "newlinesToParagraphs"),
                 SomeCustardValue $ TemplateNativeLambda
                                     cfNewlinesToParagraphs),
                
                -- Forms
                (("Base", "formInput"),
                 SomeCustardValue $ TemplateNativeLambda
                                     Forms.cfFormInput),
                
                -- Captchas
                (("Base", "generateCaptcha"),
                 SomeCustardValue $ TemplateNativeLambda
                                     Captchas.cfGenerateCaptcha),
                (("Base", "checkCaptcha"),
                 SomeCustardValue $ TemplateNativeLambda
                                     Captchas.cfCheckCaptcha),
                (("Base", "expireOldCaptchas"),
                 SomeCustardValue $ TemplateNativeLambda
                                     Captchas.cfExpireOldCaptchas),
                
                -- Everything below here is based on a corresponding function
                -- in Haskell, which may or may not have an identical name.
                
                -- Lists
                -- Lists - Basic functions
                (("Base", "head"),
                 SomeCustardValue $ TemplateNativeLambda
                                     Lists.cfHead),
                (("Base", "last"),
                 SomeCustardValue $ TemplateNativeLambda
                                     Lists.cfLast),
                (("Base", "tail"),
                 SomeCustardValue $ TemplateNativeLambda
                                     Lists.cfTail),
                (("Base", "init"),
                 SomeCustardValue $ TemplateNativeLambda
                                     Lists.cfInit),
                (("Base", "null"),
                 SomeCustardValue $ TemplateNativeLambda
                                     Lists.cfNull),
                (("Base", "length"),
                 SomeCustardValue $ TemplateNativeLambda
                                     Lists.cfLength),
                -- Lists - List transformations
                (("Base", "map"),
                 SomeCustardValue $ TemplateNativeLambda
                                     Lists.cfMap),
                (("Base", "reverse"),
                 SomeCustardValue $ TemplateNativeLambda
                                     Lists.cfReverse),
                (("Base", "intersperse"),
                 SomeCustardValue $ TemplateNativeLambda
                                     Lists.cfIntersperse),
                (("Base", "intercalate"),
                 SomeCustardValue $ TemplateNativeLambda
                                     Lists.cfIntercalate),
                (("Base", "transpose"),
                 SomeCustardValue $ TemplateNativeLambda
                                     Lists.cfTranspose),
                (("Base", "subsequences"),
                 SomeCustardValue $ TemplateNativeLambda
                                     Lists.cfSubsequences),
                (("Base", "permutations"),
                 SomeCustardValue $ TemplateNativeLambda
                                     Lists.cfPermutations),
                -- Lists - Reducing lists (folds)
                (("Base", "foldl"),
                 SomeCustardValue $ TemplateNativeLambda
                                     Lists.cfFoldl),
                (("Base", "foldl1"),
                 SomeCustardValue $ TemplateNativeLambda
                                     Lists.cfFoldl1),
                (("Base", "foldr"),
                 SomeCustardValue $ TemplateNativeLambda
                                     Lists.cfFoldr),
                (("Base", "foldr1"),
                 SomeCustardValue $ TemplateNativeLambda
                                     Lists.cfFoldr1),
                -- Lists - Reducing lists (folds) - Special folds
                (("Base", "concat"),
                 SomeCustardValue $ TemplateNativeLambda
                                     Lists.cfConcat),
                (("Base", "concatMap"),
                 SomeCustardValue $ TemplateNativeLambda
                                     Lists.cfConcatMap),
                (("Base", "and"),
                 SomeCustardValue $ TemplateNativeLambda
                                     Lists.cfAnd),
                (("Base", "or"),
                 SomeCustardValue $ TemplateNativeLambda
                                     Lists.cfOr),
                (("Base", "any"),
                 SomeCustardValue $ TemplateNativeLambda
                                     Lists.cfAny),
                (("Base", "all"),
                 SomeCustardValue $ TemplateNativeLambda
                                     Lists.cfAll),
                (("Base", "sum"),
                 SomeCustardValue $ TemplateNativeLambda
                                     Lists.cfSum),
                (("Base", "product"),
                 SomeCustardValue $ TemplateNativeLambda
                                     Lists.cfProduct),
                (("Base", "maximum"),
                 SomeCustardValue $ TemplateNativeLambda
                                     Lists.cfMaximum),
                (("Base", "minimum"),
                 SomeCustardValue $ TemplateNativeLambda
                                     Lists.cfMinimum),
                -- Lists - Building lists
                -- Lists - Building lists - Scans
                (("Base", "scanl"),
                 SomeCustardValue $ TemplateNativeLambda
                                     Lists.cfScanl),
                (("Base", "scanl1"),
                 SomeCustardValue $ TemplateNativeLambda
                                     Lists.cfScanl1),
                (("Base", "scanr"),
                 SomeCustardValue $ TemplateNativeLambda
                                     Lists.cfScanr),
                (("Base", "scanr1"),
                 SomeCustardValue $ TemplateNativeLambda
                                     Lists.cfScanr1),
                -- Lists - Building lists - Accumulating maps
                (("Base", "mapAccumL"),
                 SomeCustardValue $ TemplateNativeLambda
                                     Lists.cfMapAccumL),
                (("Base", "mapAccumR"),
                 SomeCustardValue $ TemplateNativeLambda
                                     Lists.cfMapAccumR),
                -- Lists - Building lists - Replicate
                (("Base", "replicate"),
                 SomeCustardValue $ TemplateNativeLambda
                                     Lists.cfReplicate),
                -- Lists - Building lists - Unfolding
                (("Base", "unfoldr"),
                 SomeCustardValue $ TemplateNativeLambda
                                     Lists.cfUnfoldr),
                -- Lists - Sublists
                -- Lists - Sublists - Extracting sublists
                (("Base", "take"),
                 SomeCustardValue $ TemplateNativeLambda
                                     Lists.cfTake),
                (("Base", "drop"),
                 SomeCustardValue $ TemplateNativeLambda
                                     Lists.cfDrop),
                (("Base", "splitAt"),
                 SomeCustardValue $ TemplateNativeLambda
                                     Lists.cfSplitAt),
                (("Base", "takeWhile"),
                 SomeCustardValue $ TemplateNativeLambda
                                     Lists.cfTakeWhile),
                (("Base", "dropWhile"),
                 SomeCustardValue $ TemplateNativeLambda
                                     Lists.cfDropWhile),
                (("Base", "span"),
                 SomeCustardValue $ TemplateNativeLambda
                                     Lists.cfSpan),
                (("Base", "break"),
                 SomeCustardValue $ TemplateNativeLambda
                                     Lists.cfBreak),
                (("Base", "stripPrefix"),
                 SomeCustardValue $ TemplateNativeLambda
                                     Lists.cfStripPrefix),
                (("Base", "group"),
                 SomeCustardValue $ TemplateNativeLambda
                                     Lists.cfGroup),
                (("Base", "inits"),
                 SomeCustardValue $ TemplateNativeLambda
                                     Lists.cfInits),
                (("Base", "tails"),
                 SomeCustardValue $ TemplateNativeLambda
                                     Lists.cfTails),
                -- Lists - Sublists - Predicates
                (("Base", "isPrefixOf"),
                 SomeCustardValue $ TemplateNativeLambda
                                     Lists.cfIsPrefixOf),
                (("Base", "isSuffixOf"),
                 SomeCustardValue $ TemplateNativeLambda
                                     Lists.cfIsSuffixOf),
                (("Base", "isInfixOf"),
                 SomeCustardValue $ TemplateNativeLambda
                                     Lists.cfIsInfixOf),
                -- Lists - Searching lists
                -- Lists - Searching lists - Searching by equality
                (("Base", "elem"),
                 SomeCustardValue $ TemplateNativeLambda
                                     Lists.cfElem),
                (("Base", "notElem"),
                 SomeCustardValue $ TemplateNativeLambda
                                     Lists.cfNotElem),
                (("Base", "lookup"),
                 SomeCustardValue $ TemplateNativeLambda
                                     Lists.cfLookup),
                -- Lists - Searching lists - Searching with a predicate
                (("Base", "find"),
                 SomeCustardValue $ TemplateNativeLambda
                                     Lists.cfFind),
                (("Base", "filter"),
                 SomeCustardValue $ TemplateNativeLambda
                                     Lists.cfFilter),
                (("Base", "partition"),
                 SomeCustardValue $ TemplateNativeLambda
                                     Lists.cfPartition),
                -- Lists - Indexing lists
                (("Base", "nth"),
                 SomeCustardValue $ TemplateNativeLambda
                                     Lists.cfNth),
                (("Base", "elemIndex"),
                 SomeCustardValue $ TemplateNativeLambda
                                     Lists.cfElemIndex),
                (("Base", "elemIndices"),
                 SomeCustardValue $ TemplateNativeLambda
                                     Lists.cfElemIndices),
                (("Base", "findIndex"),
                 SomeCustardValue $ TemplateNativeLambda
                                     Lists.cfFindIndex),
                (("Base", "findIndices"),
                 SomeCustardValue $ TemplateNativeLambda
                                     Lists.cfFindIndices),
                -- Lists - Special lists
                -- Lists - "Set" operations
                (("Base", "nub"),
                 SomeCustardValue $ TemplateNativeLambda
                                     Lists.cfNub),
                (("Base", "delete"),
                 SomeCustardValue $ TemplateNativeLambda
                                     Lists.cfDelete),
                (("Base", "deleteFirsts"),
                 SomeCustardValue $ TemplateNativeLambda
                                     Lists.cfDeleteFirsts),
                (("Base", "union"),
                 SomeCustardValue $ TemplateNativeLambda
                                     Lists.cfUnion),
                (("Base", "intersect"),
                 SomeCustardValue $ TemplateNativeLambda
                                     Lists.cfIntersect),
                -- Lists - Ordered lists
                (("Base", "sort"),
                 SomeCustardValue $ TemplateNativeLambda
                                     Lists.cfSort),
                (("Base", "insert"),
                 SomeCustardValue $ TemplateNativeLambda
                                     Lists.cfInsert),
                -- Lists - Generalized functions
                (("Base", "nubBy"),
                 SomeCustardValue $ TemplateNativeLambda
                                     Lists.cfNubBy),
                (("Base", "deleteBy"),
                 SomeCustardValue $ TemplateNativeLambda
                                     Lists.cfDeleteBy),
                (("Base", "deleteFirstsBy"),
                 SomeCustardValue $ TemplateNativeLambda
                                     Lists.cfDeleteFirstsBy),
                (("Base", "unionBy"),
                 SomeCustardValue $ TemplateNativeLambda
                                     Lists.cfUnionBy),
                (("Base", "intersectBy"),
                 SomeCustardValue $ TemplateNativeLambda
                                     Lists.cfIntersectBy),
                (("Base", "groupBy"),
                 SomeCustardValue $ TemplateNativeLambda
                                     Lists.cfGroupBy),
                (("Base", "sortBy"),
                 SomeCustardValue $ TemplateNativeLambda
                                     Lists.cfSortBy),
                (("Base", "insertBy"),
                 SomeCustardValue $ TemplateNativeLambda
                                     Lists.cfInsertBy),
                (("Base", "maximumBy"),
                 SomeCustardValue $ TemplateNativeLambda
                                     Lists.cfMaximumBy),
                (("Base", "minimumBy"),
                 SomeCustardValue $ TemplateNativeLambda
                                     Lists.cfMinimumBy),
                
                -- Strings
                -- Strings - Basic functions
                (("Base", "stringHead"),
                 SomeCustardValue $ TemplateNativeLambda
                                     Strings.cfStringHead),
                (("Base", "stringLast"),
                 SomeCustardValue $ TemplateNativeLambda
                                     Strings.cfStringLast),
                (("Base", "stringTail"),
                 SomeCustardValue $ TemplateNativeLambda
                                     Strings.cfStringTail),
                (("Base", "stringInit"),
                 SomeCustardValue $ TemplateNativeLambda
                                     Strings.cfStringInit),
                (("Base", "stringNull"),
                 SomeCustardValue $ TemplateNativeLambda
                                     Strings.cfStringNull),
                (("Base", "stringLength"),
                 SomeCustardValue $ TemplateNativeLambda
                                     Strings.cfStringLength),
                -- Strings - String transformations
                (("Base", "stringMap"),
                 SomeCustardValue $ TemplateNativeLambda
                                     Strings.cfStringMap),
                (("Base", "stringReverse"),
                 SomeCustardValue $ TemplateNativeLambda
                                     Strings.cfStringReverse),
                (("Base", "stringIntersperse"),
                 SomeCustardValue $ TemplateNativeLambda
                                     Strings.cfStringIntersperse),
                (("Base", "stringIntercalate"),
                 SomeCustardValue $ TemplateNativeLambda
                                     Strings.cfStringIntercalate),
                (("Base", "stringTranspose"),
                 SomeCustardValue $ TemplateNativeLambda
                                     Strings.cfStringTranspose),
                (("Base", "stringSubsequences"),
                 SomeCustardValue $ TemplateNativeLambda
                                     Strings.cfStringSubsequences),
                (("Base", "stringPermutations"),
                 SomeCustardValue $ TemplateNativeLambda
                                     Strings.cfStringPermutations),
                -- Strings - Reducing strings (folds)
                (("Base", "stringFoldl"),
                 SomeCustardValue $ TemplateNativeLambda
                                     Strings.cfStringFoldl),
                (("Base", "stringFoldl1"),
                 SomeCustardValue $ TemplateNativeLambda
                                     Strings.cfStringFoldl1),
                (("Base", "stringFoldr"),
                 SomeCustardValue $ TemplateNativeLambda
                                     Strings.cfStringFoldr),
                (("Base", "stringFoldr1"),
                 SomeCustardValue $ TemplateNativeLambda
                                     Strings.cfStringFoldr1),
                -- Strings - Reducing strings (folds) - Special folds
                (("Base", "stringConcat"),
                 SomeCustardValue $ TemplateNativeLambda
                                     Strings.cfStringConcat),
                (("Base", "stringConcatMap"),
                 SomeCustardValue $ TemplateNativeLambda
                                     Strings.cfStringConcatMap),
                (("Base", "stringAny"),
                 SomeCustardValue $ TemplateNativeLambda
                                     Strings.cfStringAny),
                (("Base", "stringAll"),
                 SomeCustardValue $ TemplateNativeLambda
                                     Strings.cfStringAll),
                -- Strings - Building strings
                -- Strings - Building strings - Scans
                (("Base", "stringScanl"),
                 SomeCustardValue $ TemplateNativeLambda
                                     Strings.cfStringScanl),
                (("Base", "stringScanl1"),
                 SomeCustardValue $ TemplateNativeLambda
                                     Strings.cfStringScanl1),
                (("Base", "stringScanr"),
                 SomeCustardValue $ TemplateNativeLambda
                                     Strings.cfStringScanr),
                (("Base", "stringScanr1"),
                 SomeCustardValue $ TemplateNativeLambda
                                     Strings.cfStringScanr1),
                -- Strings - Building strings - Accumulating maps
                (("Base", "stringMapAccumL"),
                 SomeCustardValue $ TemplateNativeLambda
                                     Strings.cfStringMapAccumL),
                (("Base", "stringMapAccumR"),
                 SomeCustardValue $ TemplateNativeLambda
                                     Strings.cfStringMapAccumR),
                -- Strings - Building strings - Replicate
                (("Base", "stringReplicate"),
                 SomeCustardValue $ TemplateNativeLambda
                                     Strings.cfStringReplicate),
                -- Strings - Building strings - Unfolding
                (("Base", "stringUnfoldr"),
                 SomeCustardValue $ TemplateNativeLambda
                                     Strings.cfStringUnfoldr),
                -- Strings - Substrings
                -- Strings - Substrings - Extracting sublists
                (("Base", "stringTake"),
                 SomeCustardValue $ TemplateNativeLambda
                                     Strings.cfStringTake),
                (("Base", "stringDrop"),
                 SomeCustardValue $ TemplateNativeLambda
                                     Strings.cfStringDrop),
                (("Base", "stringSplitAt"),
                 SomeCustardValue $ TemplateNativeLambda
                                     Strings.cfStringSplitAt),
                (("Base", "stringTakeWhile"),
                 SomeCustardValue $ TemplateNativeLambda
                                     Strings.cfStringTakeWhile),
                (("Base", "stringDropWhile"),
                 SomeCustardValue $ TemplateNativeLambda
                                     Strings.cfStringDropWhile),
                (("Base", "stringSpan"),
                 SomeCustardValue $ TemplateNativeLambda
                                     Strings.cfStringSpan),
                (("Base", "stringBreak"),
                 SomeCustardValue $ TemplateNativeLambda
                                     Strings.cfStringBreak),
                (("Base", "stringStripPrefix"),
                 SomeCustardValue $ TemplateNativeLambda
                                     Strings.cfStringStripPrefix),
                (("Base", "stringGroup"),
                 SomeCustardValue $ TemplateNativeLambda
                                     Strings.cfStringGroup),
                (("Base", "stringInits"),
                 SomeCustardValue $ TemplateNativeLambda
                                     Strings.cfStringInits),
                (("Base", "stringTails"),
                 SomeCustardValue $ TemplateNativeLambda
                                     Strings.cfStringTails),
                -- Strings - Substrings - Predicates
                (("Base", "stringIsPrefixOf"),
                 SomeCustardValue $ TemplateNativeLambda
                                     Strings.cfStringIsPrefixOf),
                (("Base", "stringIsSuffixOf"),
                 SomeCustardValue $ TemplateNativeLambda
                                     Strings.cfStringIsSuffixOf),
                (("Base", "stringIsInfixOf"),
                 SomeCustardValue $ TemplateNativeLambda
                                     Strings.cfStringIsInfixOf),
                -- Strings - Searching strings
                -- Strings - Searching strings - Searching by equality
                (("Base", "stringElem"),
                 SomeCustardValue $ TemplateNativeLambda
                                     Strings.cfStringElem),
                (("Base", "stringNotElem"),
                 SomeCustardValue $ TemplateNativeLambda
                                     Strings.cfStringNotElem),
                (("Base", "stringLookup"),
                 SomeCustardValue $ TemplateNativeLambda
                                     Strings.cfStringLookup),
                -- Strings - Searching strings - Searching with a predicate
                (("Base", "stringFind"),
                 SomeCustardValue $ TemplateNativeLambda
                                     Strings.cfStringFind),
                (("Base", "stringFilter"),
                 SomeCustardValue $ TemplateNativeLambda
                                     Strings.cfStringFilter),
                (("Base", "stringPartition"),
                 SomeCustardValue $ TemplateNativeLambda
                                     Strings.cfStringPartition),
                -- Strings - Indexing strings
                (("Base", "stringNth"),
                 SomeCustardValue $ TemplateNativeLambda
                                     Strings.cfStringNth),
                (("Base", "stringElemIndex"),
                 SomeCustardValue $ TemplateNativeLambda
                                     Strings.cfStringElemIndex),
                (("Base", "stringElemIndices"),
                 SomeCustardValue $ TemplateNativeLambda
                                     Strings.cfStringElemIndices),
                (("Base", "stringFindIndex"),
                 SomeCustardValue $ TemplateNativeLambda
                                     Strings.cfStringFindIndex),
                (("Base", "stringFindIndices"),
                 SomeCustardValue $ TemplateNativeLambda
                                     Strings.cfStringFindIndices),
                -- Strings - Text operations
                (("Base", "stringLines"),
                 SomeCustardValue $ TemplateNativeLambda
                                     Strings.cfStringLines),
                (("Base", "stringWords"),
                 SomeCustardValue $ TemplateNativeLambda
                                     Strings.cfStringWords),
                (("Base", "stringUnlines"),
                 SomeCustardValue $ TemplateNativeLambda
                                     Strings.cfStringUnlines),
                (("Base", "stringUnwords"),
                 SomeCustardValue $ TemplateNativeLambda
                                     Strings.cfStringUnwords)
               ]


cfJust :: CustardContext
       -> [AnyCustardValue]
       -> FruitTart AnyCustardValue
cfJust context parameters = do
  requireNParameters parameters 1 "just"
  return $ TemplateMaybe $ Just $ head parameters


cfParameter :: CustardContext
            -> [AnyCustardValue]
            -> FruitTart AnyCustardValue
cfParameter context parameters = do
  requireNParameters parameters 1 "parameters"
  n <- valueToInteger $ head parameters
  bindings <- getBindings
  parameters <- (return $ Map.lookup ("Base", "parameters") bindings)
                >>= valueToList . fromJust
  if n < (fromIntegral $ length parameters)
    then return $ head $ drop (fromIntegral n) parameters
    else error $ "Too few template parameters "
               ++ "for parameter(" ++ (show n) ++ ")."


cfIsNothing :: CustardContext
            -> [AnyCustardValue]
            -> FruitTart AnyCustardValue
cfIsNothing context parameters = do
  requireNParameters parameters 1 "isNothing"
  value <- return $ head parameters
  return $ case value of
             TemplateMaybe Nothing -> CustardBool True
             TemplateMaybe (Just _) -> CustardBool False
             _ -> error $ "Parameter is not a Maybe in isNothing()."


cfIsJust :: CustardContext
         -> [AnyCustardValue]
         -> FruitTart AnyCustardValue
cfIsJust context parameters = do
  requireNParameters parameters 1 "isJust"
  value <- return $ head parameters
  return $ case value of
             TemplateMaybe Nothing -> CustardBool False
             TemplateMaybe (Just _) -> CustardBool True
             _ -> error $ "Parameter is not a Maybe in isJust()."


cfFromJust :: CustardContext
           -> [AnyCustardValue]
           -> FruitTart AnyCustardValue
cfFromJust context parameters = do
  requireNParameters parameters 1 "fromJust"
  value <- return $ head parameters
  return $ case value of
             TemplateMaybe Nothing
                 -> error $ "Parameter is nothing in fromJust()."
             TemplateMaybe (Just result) -> result
             _ -> error $ "Parameter is not a Maybe in fromJust()."


cfStringWordCount :: CustardContext
                  -> [AnyCustardValue]
                  -> FruitTart AnyCustardValue
cfStringWordCount context parameters = do
  requireNParameters parameters 1 "stringWordCount"
  string <- valueToString $ head parameters
  let wordCount "" = 0
      wordCount string = case elemIndex ' ' string of
                           Nothing -> 1
                           Just index -> let (_, rest) = splitAt index string
                                         in 1 + wordCount (drop 1 rest)
  return $ TemplateInteger $ fromIntegral $ wordCount string


cfCompareIntegers :: CustardContext
                  -> [AnyCustardValue]
                  -> FruitTart AnyCustardValue
cfCompareIntegers context parameters = do
  requireNParameters parameters 2 "compareIntegers"
  a <- valueToInteger $ head parameters
  b <- valueToInteger $ head $ drop 1 parameters
  return $ TemplateOrdering $ compare a b


cfShowInteger :: CustardContext
              -> [AnyCustardValue]
              -> FruitTart AnyCustardValue
cfShowInteger context parameters = do
  requireNParameters parameters 1 "showInteger"
  integer <- valueToInteger $ head parameters
  return $ CustardString $ show integer


cfShowBool :: CustardContext
           -> [AnyCustardValue]
           -> FruitTart AnyCustardValue
cfShowBool context parameters = do
  requireNParameters parameters 1 "showBool"
  bool <- valueToBoolean $ head parameters
  return $ CustardString $ show bool


cfByteSizeToString :: CustardContext
                   -> [AnyCustardValue]
                   -> FruitTart AnyCustardValue
cfByteSizeToString context parameters = do
  requireNParameters parameters 1 "byteSizeToString"
  integer <- valueToInteger $ head parameters
  return $ CustardString $ byteSizeToString integer


cfTimestampToString :: CustardContext
                    -> [AnyCustardValue]
                    -> FruitTart AnyCustardValue
cfTimestampToString context parameters = do
  requireNParameters parameters 1 "timestampToString"
  integer <- valueToInteger $ head parameters
  return $ CustardString $ timestampToString integer


cfEscapeAttribute :: CustardContext
                  -> [AnyCustardValue]
                  -> FruitTart AnyCustardValue
cfEscapeAttribute context parameters = do
  requireNParameters parameters 1 "escapeAttribute"
  string <- valueToString $ head parameters
  return $ CustardString $ escapeAttribute string


cfEscapeHTML :: CustardContext
             -> [AnyCustardValue]
             -> FruitTart AnyCustardValue
cfEscapeHTML context parameters = do
  requireNParameters parameters 1 "escapeHTML"
  string <- valueToString $ head parameters
  return $ CustardString $ escapeHTML string


cfNewlinesToParagraphs :: CustardContext
                       -> [AnyCustardValue]
                       -> FruitTart AnyCustardValue
cfNewlinesToParagraphs context parameters = do
  requireNParameters parameters 1 "newlinesToParagraphs"
  string <- valueToString $ head parameters
  return $ CustardString $ newlinesToParagraphs string
