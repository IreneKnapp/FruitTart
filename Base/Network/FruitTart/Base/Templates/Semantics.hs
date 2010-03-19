module Network.FruitTart.Base.Templates.Semantics (
                                                   fillTemplate,
                                                   eval,
                                                   baseBindings
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

import Network.FruitTart.Base
import Network.FruitTart.Base.View.Templates
import Network.FruitTart.Base.Templates.Syntax
import Network.FruitTart.Base.Templates.Types
import Network.FruitTart.Util


fillTemplate :: String
             -> String
             -> FruitTart String
fillTemplate moduleName templateName = do
  items <- query ("SELECT template_items.kind, template_items.body "
                  ++ "FROM templates LEFT JOIN template_items "
                  ++ "ON templates.id = template_items.template "
                  ++ "WHERE templates.module = ? AND templates.name = ? "
                  ++ "ORDER BY template_items.item")
                 [SQLText moduleName, SQLText templateName]
  if items == []
    then error $ "Template " ++ moduleName ++ "." ++ templateName ++ " not found."
    else return ()
  mapM (\[SQLText kind, SQLText body] -> do
         case kind of
           "content" -> return body
           "expression" ->
               fCatch ((evalExpression $ readExpression moduleName body)
                       >>= valueToString . fst)
                      (\e -> error $ "While processing template "
                                   ++ moduleName ++ "."
                                   ++ templateName ++ ": " ++ (show (e :: SomeException)))
           _ -> error $ "Unknown template item type " ++ kind ++ ".")
       items
       >>= return . concat


eval :: String -> String -> FruitTart TemplateValue
eval moduleName body = do
  (evalExpression $ readExpression moduleName body) >>= return . fst


letBindings :: Map (String, String) TemplateValue -> FruitTart a -> FruitTart a
letBindings newBindings function = do
  bindingsMVar <- getInterfaceStateMVar "Base"
               :: FruitTart (MVar (Map (String, String) TemplateValue))
  oldBindings <- liftIO $ swapMVar bindingsMVar newBindings
  result <- fCatch function
                   (\e -> do
                            liftIO $ swapMVar bindingsMVar oldBindings
                            fThrow (e :: SomeException))
  liftIO $ swapMVar bindingsMVar oldBindings
  return result


getBindings :: FruitTart (Map (String, String) TemplateValue)
getBindings = do
  bindingsMVar <- getInterfaceStateMVar "Base"
               :: FruitTart (MVar (Map (String, String) TemplateValue))
  liftIO $ readMVar bindingsMVar


valueToBoolean :: TemplateValue -> FruitTart Bool
valueToBoolean (TemplateBool boolean) = return boolean
valueToBoolean value = error $ "Template value is not a Boolean (it's " ++ (show value)
                             ++ ")."


valueToString :: TemplateValue -> FruitTart String
valueToString (TemplateString string) = return string
valueToString value = error $ "Template value is not a String (it's " ++ (show value)
                            ++ ")."


valueToInteger :: TemplateValue -> FruitTart Int64
valueToInteger (TemplateInteger integer) = return integer
valueToInteger value = error $ "Template value is not an Integer (it's " ++ (show value)
                             ++ ")."


valueToList :: TemplateValue -> FruitTart [TemplateValue]
valueToList (TemplateList list) = return list
valueToList value = error $ "Template value is not a List (it's " ++ (show value)
                          ++ ")."


valueToMap :: TemplateValue -> FruitTart (Map (String, String) TemplateValue)
valueToMap (TemplateMap map) = return map
valueToMap value = error $ "Template value is not a Map (it's " ++ (show value)
                         ++ ")."


evalExpression :: TemplateExpression
               -> FruitTart (TemplateValue, Map (String, String) TemplateValue)
evalExpression expression = do
    case expression of
      TemplateLiteral value -> returnWithUnchangedBindings value
      TemplateExpressionList subexpressions -> do
        values <- mapM evalExpression subexpressions >>= return . (map fst)
        returnWithUnchangedBindings $ TemplateList values
      TemplateOperationConcatenate aExpression bExpression -> do
        aValue <- evalExpression aExpression >>= return . fst
        bValue <- evalExpression bExpression >>= return . fst
        case (aValue, bValue) of
          (TemplateString aString, TemplateString bString)
              -> returnWithUnchangedBindings $ TemplateString $ aString ++ bString
          _ -> error "Cannot concatenate non-Strings."
      TemplateOperationEquals aExpression bExpression -> do
        aValue <- evalExpression aExpression >>= return . fst
        bValue <- evalExpression bExpression >>= return . fst
        templateEqual aValue bValue
      TemplateOperationNotEquals aExpression bExpression -> do
        aValue <- evalExpression aExpression >>= return . fst
        bValue <- evalExpression bExpression >>= return . fst
        result <- templateEqual aValue bValue >>= return . fst
        templateNegate result
      TemplateOperationNot expression -> do
        value <- evalExpression expression >>= return . fst
        templateNegate value
      TemplateOperationAnd aExpression bExpression -> do
        aValue <- evalExpression aExpression >>= return . fst
        bValue <- evalExpression bExpression >>= return . fst
        templateAnd aValue bValue
      TemplateOperationOr aExpression bExpression -> do
        aValue <- evalExpression aExpression >>= return . fst
        bValue <- evalExpression bExpression >>= return . fst
        templateOr aValue bValue
      TemplateOperationGreaterEquals aExpression bExpression -> do
        aValue <- evalExpression aExpression >>= return . fst
        bValue <- evalExpression bExpression >>= return . fst
        result <- templateCompare aValue bValue
        returnWithUnchangedBindings $ TemplateBool $ case result of
                                                       EQ -> True
                                                       GT -> True
                                                       LT -> False
      TemplateOperationGreater aExpression bExpression -> do
        aValue <- evalExpression aExpression >>= return . fst
        bValue <- evalExpression bExpression >>= return . fst
        result <- templateCompare aValue bValue
        returnWithUnchangedBindings $ TemplateBool $ case result of
                                                       EQ -> False
                                                       GT -> True
                                                       LT -> False
      TemplateOperationLessEquals aExpression bExpression -> do
        aValue <- evalExpression aExpression >>= return . fst
        bValue <- evalExpression bExpression >>= return . fst
        result <- templateCompare aValue bValue
        returnWithUnchangedBindings $ TemplateBool $ case result of
                                                       EQ -> True
                                                       GT -> False
                                                       LT -> True
      TemplateOperationLess aExpression bExpression -> do
        aValue <- evalExpression aExpression >>= return . fst
        bValue <- evalExpression bExpression >>= return . fst
        result <- templateCompare aValue bValue
        returnWithUnchangedBindings $ TemplateBool $ case result of
                                                       EQ -> False
                                                       GT -> False
                                                       LT -> True
      TemplateOperationAdd aExpression bExpression -> do
        aValue <- evalExpression aExpression >>= return . fst
        bValue <- evalExpression bExpression >>= return . fst
        templateArithmetic aValue bValue (+)
      TemplateOperationSubtract aExpression bExpression -> do
        aValue <- evalExpression aExpression >>= return . fst
        bValue <- evalExpression bExpression >>= return . fst
        templateArithmetic aValue bValue (-)
      TemplateOperationMultiply aExpression bExpression -> do
        aValue <- evalExpression aExpression >>= return . fst
        bValue <- evalExpression bExpression >>= return . fst
        templateArithmetic aValue bValue (*)
      TemplateOperationDivide aExpression bExpression -> do
        aValue <- evalExpression aExpression >>= return . fst
        bValue <- evalExpression bExpression >>= return . fst
        templateArithmetic aValue bValue div
      TemplateIfExpression subexpressions -> do
        if length subexpressions /= 3
          then error $ "Invalid number of parameters to if()."
          else return ()
        let condition = head subexpressions
            ifTrue = head $ drop 1 subexpressions
            ifFalse = head $ drop 2 subexpressions
        result <- evalExpression condition
                  >>= valueToBoolean . fst
        if result
          then evalExpression ifTrue
          else evalExpression ifFalse
      TemplateCaseExpression subexpressions -> do
        let n = length subexpressions
        if not ((n > 1) && (odd n))
          then error $ "Invalid number of parameters to case()."
          else return ()
        mainKey <- (evalExpression $ head subexpressions) >>= return . fst
        let case' items = do
              case items of
                [] -> error $ "No match in case() for " ++ (show mainKey) ++ "."
                (key:(value:rest)) -> do
                  case key of
                    TemplateVariable (_, "otherwise") ->
                      evalExpression value
                    _ -> do
                      key <- evalExpression key >>= return . fst
                      if mainKey == key
                        then evalExpression value
                        else case' rest
        case' $ tail subexpressions
      TemplateCallExpression subexpressions -> do
        if length subexpressions < 1
           then error $ "Invalid number of parameters to call()."
           else return ()
        subparameters <- (mapM evalExpression $ tail subexpressions)
                         >>= return . map fst
        oldBindings <- getBindings
        let newBindings = Map.fromList [(("Templates", "parameters"),
                                         TemplateList subparameters)]
            subbindings = Map.union newBindings oldBindings
        (moduleName, templateName)
            <- case head subexpressions of
                 TemplateVariable result -> return result
                 _ -> error $ "First parameter is not a variable to call()."
        result <- letBindings subbindings
                              $ fillTemplate moduleName templateName
        returnWithUnchangedBindings $ TemplateString result
      TemplateIterateExpression subexpressions -> do
        if length subexpressions < 2
           then error $ "Invalid number of parameters to iterate()."
           else return ()
        subparameters <- (mapM evalExpression $ drop 2 subexpressions)
                         >>= return . map fst
        oldBindings <- getBindings
        let newBindings = Map.fromList [(("Templates", "parameters"),
                                         TemplateList subparameters)]
            globalSubbindings = Map.union newBindings oldBindings
        (moduleName, templateName)
            <- case head subexpressions of
                 TemplateVariable result -> return result
                 _ -> error $ "First parameter to iterate() is not a variable."
        rows <- (evalExpression $ head $ drop 1 subexpressions)
                >>= valueToList . fst
        results <- mapM (\row -> do
                           row' <- valueToMap row
                           let localSubbindings = Map.union row' globalSubbindings
                           letBindings localSubbindings
                                       $ fillTemplate moduleName templateName)
                        rows
        returnWithUnchangedBindings $ TemplateString $ concat results
      TemplateQueryExpression subexpressions -> do
        if length subexpressions < 1
           then error $ "Invalid number of parameters to query()."
           else return ()
        parameters <- (mapM evalExpression $ drop 1 subexpressions)
                      >>= return . map fst
        (moduleName, queryName)
            <- case head subexpressions of
                 TemplateVariable result -> return result
                 _ -> error $ "First parameter to query() is not a variable."
        results <- mapM (\parameter -> case parameter of
                           TemplateBool True -> return $ SQLInteger 1
                           TemplateBool False -> return $ SQLInteger 0
                           TemplateMaybe Nothing -> return $ SQLNull
                           TemplateMaybe (Just (TemplateInteger integer))
                               -> return $ SQLInteger integer
                           TemplateMaybe (Just (TemplateString string))
                               -> return $ SQLText string
                           TemplateInteger integer -> return $ SQLInteger integer
                           TemplateString string -> return $ SQLText string
                           _ -> error "Invalid type for query parameter.")
                        parameters
                   >>= namedQuery moduleName queryName
                   >>= return . map TemplateMap
        returnWithUnchangedBindings $ TemplateList results
      TemplateLookupExpression subexpressions -> do
        if length subexpressions /= 2
           then error $ "Invalid number of parameters to lookup()."
           else return ()
        theMap <- (evalExpression $ head $ drop 1 subexpressions)
                  >>= valueToMap . fst
        (moduleName, queryName)
            <- case head subexpressions of
                 TemplateVariable result -> return result
                 _ -> error $ "First parameter to lookup() is not a variable."
        returnWithUnchangedBindings
          $ fromJust $ Map.lookup (moduleName, queryName) theMap 
      TemplateBoundExpression subexpressions -> do
        if length subexpressions /= 1
          then error $ "Invalid number of parameters to bound()."
          else return ()
        (moduleName, variableName)
            <- case head subexpressions of
                 TemplateVariable result -> return result
                 _ -> error $ "Parameter to bound() is not a variable."
        bindings <- getBindings
        returnWithUnchangedBindings
          $ case Map.lookup (moduleName, variableName) bindings of
             Nothing -> TemplateBool False
             Just _ -> TemplateBool True
      TemplateFunctionCall functionExpression actualParameterExpressions -> do
        function <- evalExpression functionExpression
                    >>= return . fst
        actualParameters <- mapM evalExpression actualParameterExpressions
                            >>= return . map fst
        result <- applyFunction function actualParameters
        returnWithUnchangedBindings result
      TemplateLambdaExpression formalParameters body -> do
        bindings <- getBindings
        returnWithUnchangedBindings $ TemplateLambda formalParameters bindings body
      TemplateVariable variableName@(packageName, properName) -> do
        bindings <- getBindings
        case Map.lookup variableName bindings of
          Nothing -> case Map.lookup ("Templates", properName) bindings of
                       Nothing -> error $ "Undefined variable " ++ packageName
                                        ++ "." ++ properName ++ "."
                       Just value -> returnWithUnchangedBindings value
          Just value -> returnWithUnchangedBindings value
      TemplateBindExpression subexpressions -> do
        if length subexpressions /= 2
          then error $ "Invalid number of parameters to bind()."
          else return ()
        (moduleName, variableName)
            <- case head subexpressions of
                 TemplateVariable result -> return result
                 _ -> error $ "Parameter to bind() is not a variable."
        value <- (evalExpression $ head $ drop 1 subexpressions)
                 >>= return . fst
        oldBindings <- getBindings
        let newBindings = Map.union (Map.fromList [((moduleName, variableName),
                                                    value)])
                                    oldBindings
        return $ (TemplateNull, newBindings)
      TemplateSequence expressionA expressionB -> do
        (_, temporaryBindings) <- evalExpression expressionA
        letBindings temporaryBindings $ evalExpression expressionB


templateEqual :: TemplateValue
              -> TemplateValue
              -> FruitTart (TemplateValue, Map (String, String) TemplateValue)
templateEqual (TemplateBool a) (TemplateBool b)
    = returnWithUnchangedBindings $ TemplateBool $ a == b
templateEqual (TemplateInteger a) (TemplateInteger b)
    = returnWithUnchangedBindings $ TemplateBool $ a == b
templateEqual (TemplateString a) (TemplateString b)
    = returnWithUnchangedBindings $ TemplateBool $ a == b
templateEqual (TemplateOrdering a) (TemplateOrdering b)
    = returnWithUnchangedBindings $ TemplateBool $ a == b
templateEqual _ _
    = error $ "Template values in comparison are not the same type or "
            ++ "are not Booleans, Integers, Strings, or Orderings."


templateNegate :: TemplateValue
               -> FruitTart (TemplateValue, Map (String, String) TemplateValue)
templateNegate (TemplateBool value)
    = returnWithUnchangedBindings $ TemplateBool $ not value
templateNegate _ = error "Template value in logical operation is not a Boolean."


templateAnd :: TemplateValue
            -> TemplateValue
            -> FruitTart (TemplateValue, Map (String, String) TemplateValue)
templateAnd (TemplateBool a) (TemplateBool b)
    = returnWithUnchangedBindings $ TemplateBool $ a && b
templateAnd _ _ = error "Template values in logical operation are not both Booleans."


templateOr :: TemplateValue
           -> TemplateValue
           -> FruitTart (TemplateValue, Map (String, String) TemplateValue)
templateOr (TemplateBool a) (TemplateBool b)
    = returnWithUnchangedBindings $ TemplateBool $ a || b
templateOr _ _ = error "Template values in logical operation are not both Booleans."


templateCompare :: TemplateValue -> TemplateValue -> FruitTart Ordering
templateCompare (TemplateInteger a) (TemplateInteger b) = return $ compare a b
templateCompare _ _ = error "Template values in comparison are not both Integers."


templateArithmetic :: TemplateValue
                   -> TemplateValue
                   -> (Int64 -> Int64 -> Int64)
                   -> FruitTart (TemplateValue, Map (String, String) TemplateValue)
templateArithmetic (TemplateInteger a) (TemplateInteger b) operation
    = returnWithUnchangedBindings $ TemplateInteger $ operation a b
templateArithmetic _ _ _ = error "Template values in arithmetic are not both Integers."


returnWithUnchangedBindings :: a -> FruitTart (a, Map (String, String) TemplateValue)
returnWithUnchangedBindings a = do
  unchangedBindings <- getBindings
  return (a, unchangedBindings)


applyFunction :: TemplateValue
              -> [TemplateValue]
              -> FruitTart TemplateValue
applyFunction function actualParameters = do
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
      letBindings subbindings (evalExpression body >>= return . fst)
    TemplateNativeLambda body -> do
      body actualParameters
    _ -> do
      error $ "Call to something not a function."


baseBindings :: Map (String, String) TemplateValue
baseBindings = Map.fromList
               [(("Templates", "parameters"), TemplateList []),
                (("Templates", "Nothing"), TemplateMaybe Nothing),
                (("Templates", "Just"), TemplateNativeLambda tfJust),
                (("Templates", "LT"), TemplateOrdering LT),
                (("Templates", "GT"), TemplateOrdering GT),
                (("Templates", "EQ"), TemplateOrdering EQ),
                (("Templates", "parameter"), TemplateNativeLambda tfParameter),
                (("Templates", "isNothing"), TemplateNativeLambda tfIsNothing),
                (("Templates", "isJust"), TemplateNativeLambda tfIsJust),
                (("Templates", "fromJust"), TemplateNativeLambda tfFromJust),
                (("Templates", "stringLength"), TemplateNativeLambda tfStringLength),
                (("Templates", "stringWordCount"),
                 TemplateNativeLambda tfStringWordCount),
                (("Templates", "length"), TemplateNativeLambda tfLength),
                (("Templates", "concat"), TemplateNativeLambda tfConcat),
                (("Templates", "intercalate"), TemplateNativeLambda tfIntercalate),
                (("Templates", "map"), TemplateNativeLambda tfMap),
                (("Tempaltes", "groupBy"), TemplateNativeLambda tfGroupBy),
                (("Templates", "mergeBy"), TemplateNativeLambda tfMergeBy),
                (("Templates", "compareIntegers"),
                 TemplateNativeLambda tfCompareIntegers),
                (("Templates", "showInteger"), TemplateNativeLambda tfShowInteger),
                (("Templates", "byteSizeToString"),
                 TemplateNativeLambda tfByteSizeToString),
                (("Templates", "timestampToString"),
                 TemplateNativeLambda tfTimestampToString),
                (("Templates", "escapeAttribute"),
                 TemplateNativeLambda tfEscapeAttribute),
                (("Templates", "escapeHTML"), TemplateNativeLambda tfEscapeHTML),
                (("Templates", "newlinesToParagraphs"),
                 TemplateNativeLambda tfNewlinesToParagraphs)]


tfJust :: [TemplateValue]
       -> FruitTart TemplateValue
tfJust parameters = do
  requireNParameters parameters 1 "just"
  return $ TemplateMaybe $ Just $ head parameters


tfParameter :: [TemplateValue]
            -> FruitTart TemplateValue
tfParameter parameters = do
  requireNParameters parameters 1 "parameters"
  n <- valueToInteger $ head parameters
  bindings <- getBindings
  parameters <- (return $ Map.lookup ("Templates", "parameters") bindings)
                >>= valueToList . fromJust
  if n < (fromIntegral $ length parameters)
    then return $ head $ drop (fromIntegral n) parameters
    else error $ "Too few template parameters "
               ++ "for parameter(" ++ (show n) ++ ")."


tfIsNothing :: [TemplateValue]
            -> FruitTart TemplateValue
tfIsNothing parameters = do
  requireNParameters parameters 1 "isNothing"
  value <- return $ head parameters
  return $ case value of
             TemplateMaybe Nothing -> TemplateBool True
             TemplateMaybe (Just _) -> TemplateBool False
             _ -> error $ "Parameter is not a Maybe in isNothing()."


tfIsJust :: [TemplateValue]
         -> FruitTart TemplateValue
tfIsJust parameters = do
  requireNParameters parameters 1 "isJust"
  value <- return $ head parameters
  return $ case value of
             TemplateMaybe Nothing -> TemplateBool False
             TemplateMaybe (Just _) -> TemplateBool True
             _ -> error $ "Parameter is not a Maybe in isJust()."


tfFromJust :: [TemplateValue]
           -> FruitTart TemplateValue
tfFromJust parameters = do
  requireNParameters parameters 1 "fromJust"
  value <- return $ head parameters
  return $ case value of
             TemplateMaybe Nothing
                 -> error $ "Parameter is nothing in fromJust()."
             TemplateMaybe (Just result) -> result
             _ -> error $ "Parameter is not a Maybe in fromJust()."


tfStringLength :: [TemplateValue]
               -> FruitTart TemplateValue
tfStringLength parameters = do
  requireNParameters parameters 1 "stringLength"
  string <- valueToString $ head parameters
  return $ TemplateInteger $ fromIntegral $ length string


tfStringWordCount :: [TemplateValue]
               -> FruitTart TemplateValue
tfStringWordCount parameters = do
  requireNParameters parameters 1 "stringWordCount"
  string <- valueToString $ head parameters
  let wordCount "" = 0
      wordCount string = case elemIndex ' ' string of
                           Nothing -> 1
                           Just index -> let (_, rest) = splitAt index string
                                         in 1 + wordCount (drop 1 rest)
  return $ TemplateInteger $ fromIntegral $ wordCount string


tfLength :: [TemplateValue]
         -> FruitTart TemplateValue
tfLength parameters = do
  requireNParameters parameters 1 "length"
  list <- valueToList $ head parameters
  return $ TemplateInteger $ fromIntegral $ length list


tfConcat :: [TemplateValue]
         -> FruitTart TemplateValue
tfConcat parameters = do
  requireNParameters parameters 1 "concat"
  list <- valueToList $ head parameters
  sublists <- mapM valueToList list
  return $ TemplateList $ concat sublists


tfIntercalate :: [TemplateValue]
              -> FruitTart TemplateValue
tfIntercalate parameters = do
  requireNParameters parameters 2 "intercalate"
  string <- valueToString $ head parameters
  list <- valueToList $ head $ drop 1 parameters
  strings <- mapM valueToString list
  return $ TemplateString $ intercalate string strings


tfMap :: [TemplateValue]
      -> FruitTart TemplateValue
tfMap parameters = do
  requireNParameters parameters 2 "map"
  let function = head parameters
  list <- valueToList $ head $ drop 1 parameters
  list' <- mapM (\a -> applyFunction function [a]) list
  return $ TemplateList list'


tfGroupBy :: [TemplateValue]
          -> FruitTart TemplateValue
tfGroupBy parameters = do
  requireNParameters parameters 2 "mergeBy"
  let function = head parameters
  list <- valueToList $ head $ drop 1 parameters
  groupedLists <- groupByM (\a b -> do
                              TemplateBool bool
                                  <- applyFunction function [a, b]
                              return bool)
                           list
  return $ TemplateList list


tfMergeBy :: [TemplateValue]
          -> FruitTart TemplateValue
tfMergeBy parameters = do
  requireNParameters parameters 2 "mergeBy"
  let function = head parameters
  lists <- (valueToList $ head $ drop 1 parameters)
           >>= mapM valueToList
  mergedLists <- mergeByM (\a b -> do
                             TemplateOrdering ordering
                                 <- applyFunction function [a, b]
                             return ordering)
                          lists
  return $ TemplateList mergedLists


tfCompareIntegers :: [TemplateValue]
                  -> FruitTart TemplateValue
tfCompareIntegers parameters = do
  requireNParameters parameters 2 "compareIntegers"
  a <- valueToInteger $ head parameters
  b <- valueToInteger $ head $ drop 1 parameters
  return $ TemplateOrdering $ compare a b


tfShowInteger :: [TemplateValue]
              -> FruitTart TemplateValue
tfShowInteger parameters = do
  requireNParameters parameters 1 "showInteger"
  integer <- valueToInteger $ head parameters
  return $ TemplateString $ show integer


tfByteSizeToString :: [TemplateValue]
                   -> FruitTart TemplateValue
tfByteSizeToString parameters = do
  requireNParameters parameters 1 "byteSizeToString"
  integer <- valueToInteger $ head parameters
  return $ TemplateString $ byteSizeToString integer


tfTimestampToString :: [TemplateValue]
                    -> FruitTart TemplateValue
tfTimestampToString parameters = do
  requireNParameters parameters 1 "timestampToString"
  integer <- valueToInteger $ head parameters
  return $ TemplateString $ timestampToString integer


tfEscapeAttribute :: [TemplateValue]
                  -> FruitTart TemplateValue
tfEscapeAttribute parameters = do
  requireNParameters parameters 1 "escapeAttribute"
  string <- valueToString $ head parameters
  return $ TemplateString $ escapeAttribute string


tfEscapeHTML :: [TemplateValue]
             -> FruitTart TemplateValue
tfEscapeHTML parameters = do
  requireNParameters parameters 1 "escapeHTML"
  string <- valueToString $ head parameters
  return $ TemplateString $ escapeHTML string


tfNewlinesToParagraphs :: [TemplateValue]
                       -> FruitTart TemplateValue
tfNewlinesToParagraphs parameters = do
  requireNParameters parameters 1 "newlinesToParagraphs"
  string <- valueToString $ head parameters
  return $ TemplateString $ newlinesToParagraphs string


requireNParameters :: [TemplateValue] -> Int -> String -> FruitTart ()
requireNParameters parameters n functionName = do
  if length parameters /= n
    then error $ "Invalid number of parameters to " ++ functionName ++ "()."
    else return ()
