module Network.FruitTart.Base.Templates.Semantics (
                                                   fillTemplate,
                                                   baseBindings
                                                  )
    where

import Control.Exception
import Control.Monad.State
import Data.List
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import Network.CGI.Monad
import Prelude hiding (catch)

import Network.FruitTart.Base
import Network.FruitTart.Base.Templates.Syntax
import Network.FruitTart.Base.Templates.Types
import Network.FruitTart.Util


fillTemplate :: String
             -> String
             -> (Map (String, String) TemplateValue)
             -> FruitTart String
fillTemplate moduleName templateName bindings = do
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
               catchFruitTart ((evalExpression bindings
                                               $ readExpression moduleName body)
                               >>= valueToString)
                              (\e -> error $ "While processing template "
                                           ++ moduleName ++ "."
                                           ++ templateName ++ ": " ++ (show e))
           _ -> error $ "Unknown template item type " ++ kind ++ ".")
       items
       >>= return . concat


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


evalExpression :: (Map (String, String) TemplateValue)
               -> TemplateExpression
               -> FruitTart TemplateValue
evalExpression bindings expression = do
    case expression of
      TemplateLiteral value -> return value
      TemplateExpressionList subexpressions -> do
        values <- mapM (evalExpression bindings)
                       subexpressions
        return $ TemplateList values
      TemplateOperationConcatenate aExpression bExpression -> do
        aValue <- evalExpression bindings aExpression
        bValue <- evalExpression bindings bExpression
        case (aValue, bValue) of
          (TemplateString aString, TemplateString bString)
              -> return $ TemplateString $ aString ++ bString
          _ -> error "Cannot concatenate non-Strings."
      TemplateOperationEquals aExpression bExpression -> do
        aValue <- evalExpression bindings aExpression
        bValue <- evalExpression bindings bExpression
        templateEqual aValue bValue
      TemplateOperationNotEquals aExpression bExpression -> do
        aValue <- evalExpression bindings aExpression
        bValue <- evalExpression bindings bExpression
        result <- templateEqual aValue bValue
        templateNegate result
      TemplateOperationNot expression -> do
        value <- evalExpression bindings expression
        templateNegate value
      TemplateOperationAnd aExpression bExpression -> do
        aValue <- evalExpression bindings aExpression
        bValue <- evalExpression bindings bExpression
        templateAnd aValue bValue
      TemplateOperationOr aExpression bExpression -> do
        aValue <- evalExpression bindings aExpression
        bValue <- evalExpression bindings bExpression
        templateOr aValue bValue
      TemplateOperationGreaterEquals aExpression bExpression -> do
        aValue <- evalExpression bindings aExpression
        bValue <- evalExpression bindings bExpression
        result <- templateCompare aValue bValue
        return $ TemplateBool $ case result of
                                  EQ -> True
                                  GT -> True
                                  LT -> False
      TemplateOperationGreater aExpression bExpression -> do
        aValue <- evalExpression bindings aExpression
        bValue <- evalExpression bindings bExpression
        result <- templateCompare aValue bValue
        return $ TemplateBool $ case result of
                                  EQ -> False
                                  GT -> True
                                  LT -> False
      TemplateOperationLessEquals aExpression bExpression -> do
        aValue <- evalExpression bindings aExpression
        bValue <- evalExpression bindings bExpression
        result <- templateCompare aValue bValue
        return $ TemplateBool $ case result of
                                  EQ -> True
                                  GT -> False
                                  LT -> True
      TemplateOperationLess aExpression bExpression -> do
        aValue <- evalExpression bindings aExpression
        bValue <- evalExpression bindings bExpression
        result <- templateCompare aValue bValue
        return $ TemplateBool $ case result of
                                  EQ -> False
                                  GT -> False
                                  LT -> True
      TemplateOperationAdd aExpression bExpression -> do
        aValue <- evalExpression bindings aExpression
        bValue <- evalExpression bindings bExpression
        templateArithmetic aValue bValue (+)
      TemplateOperationSubtract aExpression bExpression -> do
        aValue <- evalExpression bindings aExpression
        bValue <- evalExpression bindings bExpression
        templateArithmetic aValue bValue (-)
      TemplateOperationMultiply aExpression bExpression -> do
        aValue <- evalExpression bindings aExpression
        bValue <- evalExpression bindings bExpression
        templateArithmetic aValue bValue (*)
      TemplateOperationDivide aExpression bExpression -> do
        aValue <- evalExpression bindings aExpression
        bValue <- evalExpression bindings bExpression
        templateArithmetic aValue bValue div
      TemplateIfExpression subexpressions -> do
        if length subexpressions /= 3
          then error $ "Invalid number of parameters to if()."
          else return ()
        let condition = head subexpressions
            ifTrue = head $ drop 1 subexpressions
            ifFalse = head $ drop 2 subexpressions
        result <- evalExpression bindings condition
                  >>= valueToBoolean
        if result
          then evalExpression bindings ifTrue
          else evalExpression bindings ifFalse
      TemplateCaseExpression subexpressions -> do
        let n = length subexpressions
        if not ((n > 1) && (odd n))
          then error $ "Invalid number of parameters to case()."
          else return ()
        mainKey <- evalExpression bindings
                                  $ head subexpressions
        let case' items = do
              case items of
                [] -> error $ "No match in case()."
                (key:(value:rest)) -> do
                  case key of
                    TemplateVariable (_, "otherwise") ->
                      evalExpression bindings value
                    _ -> do
                      key <- evalExpression bindings key
                      if mainKey == key
                        then evalExpression bindings value
                        else case' rest
        case' $ tail subexpressions
      TemplateCallExpression subexpressions -> do
        if length subexpressions < 1
           then error $ "Invalid number of parameters to call()."
           else return ()
        subparameters <- mapM (evalExpression bindings)
                              $ tail subexpressions
        let newBindings = Map.fromList [(("Templates", "parameters"),
                                         TemplateList subparameters)]
            subbindings = Map.union newBindings bindings
        (moduleName, templateName)
            <- case head subexpressions of
                 TemplateVariable result -> return result
                 _ -> error $ "First parameter is not a variable to call()."
        result <- fillTemplate moduleName templateName subbindings
        return $ TemplateString result
      TemplateIterateExpression subexpressions -> do
        if length subexpressions < 2
           then error $ "Invalid number of parameters to iterate()."
           else return ()
        subparameters <- mapM (evalExpression bindings)
                              $ drop 2 subexpressions
        let newBindings = Map.fromList [(("Templates", "parameters"),
                                         TemplateList subparameters)]
            globalSubbindings = Map.union newBindings bindings
        (moduleName, templateName)
            <- case head subexpressions of
                 TemplateVariable result -> return result
                 _ -> error $ "First parameter to iterate() is not a variable."
        rows <- (evalExpression bindings $ head $ drop 1 subexpressions)
                >>= valueToList
        results <- mapM (\row -> do
                           row' <- valueToMap row
                           let localSubbindings = Map.union row' globalSubbindings
                           fillTemplate moduleName templateName localSubbindings)
                        rows
        return $ TemplateString $ concat results
      TemplateBoundExpression subexpressions -> do
        if length subexpressions /= 1
          then error $ "Invalid number of parameters to bound()."
          else return ()
        (moduleName, variableName)
            <- case head subexpressions of
                 TemplateVariable result -> return result
                 _ -> error $ "Parameter to bound() is not a variable."
        return $ case Map.lookup (moduleName, variableName) bindings of
             Nothing -> TemplateBool False
             Just _ -> TemplateBool True
      TemplateFunctionCall functionExpression actualParameterExpressions -> do
        function <- evalExpression bindings functionExpression
        actualParameters <- mapM (\actualParameter ->
                                    evalExpression bindings actualParameter)
                                 actualParameterExpressions
        applyFunction bindings function actualParameters
      TemplateLambdaExpression formalParameters body -> do
        return $ TemplateLambda formalParameters bindings body
      TemplateVariable variableName@(packageName, properName) -> do
        case Map.lookup variableName bindings of
          Nothing -> case Map.lookup ("Templates", properName) bindings of
                       Nothing -> error $ "Undefined variable " ++ packageName
                                        ++ "." ++ properName ++ "."
                       Just value -> return value
          Just value -> return value
      TemplateSequence expressionA expressionB -> do
        evalExpression bindings expressionA
        evalExpression bindings expressionB


templateEqual :: TemplateValue -> TemplateValue -> FruitTart TemplateValue
templateEqual (TemplateBool a) (TemplateBool b) = return $ TemplateBool $ a == b
templateEqual (TemplateInteger a) (TemplateInteger b) = return $ TemplateBool $ a == b
templateEqual (TemplateString a) (TemplateString b) = return $ TemplateBool $ a == b
templateEqual (TemplateOrdering a) (TemplateOrdering b) = return $ TemplateBool $ a == b
templateEqual _ _
    = error $ "Template values in comparison are not the same type or "
            ++ "are not Booleans, Integers, Strings, or Orderings."


templateNegate :: TemplateValue -> FruitTart TemplateValue
templateNegate (TemplateBool value) = return $ TemplateBool $ not value
templateNegate _ = error "Template value in logical operation is not a Boolean."


templateAnd :: TemplateValue -> TemplateValue -> FruitTart TemplateValue
templateAnd (TemplateBool a) (TemplateBool b) = return $ TemplateBool $ a && b
templateAnd _ _ = error "Template values in logical operation are not both Booleans."


templateOr :: TemplateValue -> TemplateValue -> FruitTart TemplateValue
templateOr (TemplateBool a) (TemplateBool b) = return $ TemplateBool $ a || b
templateOr _ _ = error "Template values in logical operation are not both Booleans."


templateCompare :: TemplateValue -> TemplateValue -> FruitTart Ordering
templateCompare (TemplateInteger a) (TemplateInteger b) = return $ compare a b
templateCompare _ _ = error "Template values in comparison are not both Integers."


templateArithmetic :: TemplateValue -> TemplateValue -> (Int64 -> Int64 -> Int64)
                   -> FruitTart TemplateValue
templateArithmetic (TemplateInteger a) (TemplateInteger b) operation
    = return $ TemplateInteger $ operation a b
templateArithmetic _ _ _ = error "Template values in arithmetic are not both Integers."


applyFunction :: Map (String, String) TemplateValue
              -> TemplateValue
              -> [TemplateValue]
              -> FruitTart TemplateValue
applyFunction bindings function actualParameters = do
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
      evalExpression subbindings body
    TemplateNativeLambda body -> do
      body bindings actualParameters
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
                (("Templates", "length"), TemplateNativeLambda tfLength),
                (("Templates", "concat"), TemplateNativeLambda tfConcat),
                (("Templates", "intercalate"), TemplateNativeLambda tfIntercalate),
                (("Templates", "map"), TemplateNativeLambda tfMap),
                (("Templates", "mergeBy"), TemplateNativeLambda tfMergeBy),
                (("Templates", "compare"), TemplateNativeLambda tfCompare),
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


tfJust :: Map (String, String) TemplateValue
       -> [TemplateValue]
       -> FruitTart TemplateValue
tfJust bindings parameters = do
  requireNParameters parameters 1 "just"
  return $ TemplateMaybe $ Just $ head parameters


tfParameter :: Map (String, String) TemplateValue
             -> [TemplateValue]
             -> FruitTart TemplateValue
tfParameter bindings parameters = do
  requireNParameters parameters 1 "parameters"
  n <- valueToInteger $ head parameters
  parameters <- (return $ Map.lookup ("Templates", "parameters") bindings)
                >>= valueToList . fromJust
  if n < (fromIntegral $ length parameters)
    then return $ head $ drop (fromIntegral n) parameters
    else error $ "Too few template parameters "
               ++ "for parameter(" ++ (show n) ++ ")."


tfIsNothing :: Map (String, String) TemplateValue
            -> [TemplateValue]
            -> FruitTart TemplateValue
tfIsNothing bindings parameters = do
  requireNParameters parameters 1 "isNothing"
  value <- return $ head parameters
  return $ case value of
             TemplateMaybe Nothing -> TemplateBool True
             TemplateMaybe (Just _) -> TemplateBool False
             _ -> error $ "Parameter is not a Maybe in isNothing()."


tfIsJust :: Map (String, String) TemplateValue
         -> [TemplateValue]
         -> FruitTart TemplateValue
tfIsJust bindings parameters = do
  requireNParameters parameters 1 "isJust"
  value <- return $ head parameters
  return $ case value of
             TemplateMaybe Nothing -> TemplateBool False
             TemplateMaybe (Just _) -> TemplateBool True
             _ -> error $ "Parameter is not a Maybe in isJust()."


tfFromJust :: Map (String, String) TemplateValue
           -> [TemplateValue]
           -> FruitTart TemplateValue
tfFromJust bindings parameters = do
  requireNParameters parameters 1 "fromJust"
  value <- return $ head parameters
  return $ case value of
             TemplateMaybe Nothing
                 -> error $ "Parameter is nothing in fromJust()."
             TemplateMaybe (Just result) -> result
             _ -> error $ "Parameter is not a Maybe in fromJust()."


tfStringLength :: Map (String, String) TemplateValue
               -> [TemplateValue]
               -> FruitTart TemplateValue
tfStringLength bindings parameters = do
  requireNParameters parameters 1 "stringLength"
  string <- valueToString $ head parameters
  return $ TemplateInteger $ fromIntegral $ length string


tfLength :: Map (String, String) TemplateValue
         -> [TemplateValue]
         -> FruitTart TemplateValue
tfLength bindings parameters = do
  requireNParameters parameters 1 "length"
  list <- valueToList $ head parameters
  return $ TemplateInteger $ fromIntegral $ length list


tfConcat :: Map (String, String) TemplateValue
         -> [TemplateValue]
         -> FruitTart TemplateValue
tfConcat bindings parameters = do
  requireNParameters parameters 1 "concat"
  list <- valueToList $ head parameters
  sublists <- mapM valueToList list
  return $ TemplateList $ concat sublists


tfIntercalate :: Map (String, String) TemplateValue
              -> [TemplateValue]
              -> FruitTart TemplateValue
tfIntercalate bindings parameters = do
  requireNParameters parameters 2 "intercalate"
  string <- valueToString $ head parameters
  list <- valueToList $ head $ drop 1 parameters
  strings <- mapM valueToString list
  return $ TemplateString $ intercalate string strings


tfMap :: Map (String, String) TemplateValue
      -> [TemplateValue]
      -> FruitTart TemplateValue
tfMap bindings parameters = do
  requireNParameters parameters 2 "map"
  let function = head parameters
  list <- valueToList $ head $ drop 1 parameters
  list' <- mapM (\a -> applyFunction bindings function [a]) list
  return $ TemplateList list'


tfMergeBy :: Map (String, String) TemplateValue
          -> [TemplateValue]
          -> FruitTart TemplateValue
tfMergeBy bindings parameters = do
  requireNParameters parameters 2 "mergeBy"
  let function = head parameters
  lists <- (valueToList $ head $ drop 1 parameters)
           >>= mapM valueToList
  mergedLists <- mergeByM (\a b -> do
                             TemplateOrdering ordering
                                 <- applyFunction bindings function [a, b]
                             return ordering)
                          lists
  return $ TemplateList mergedLists


tfCompare :: Map (String, String) TemplateValue
          -> [TemplateValue]
          -> FruitTart TemplateValue
tfCompare bindings parameters = do
  requireNParameters parameters 2 "compare"
  a <- valueToInteger $ head parameters
  b <- valueToInteger $ head $ drop 1 parameters
  return $ TemplateOrdering $ compare a b


tfShowInteger :: Map (String, String) TemplateValue
              -> [TemplateValue]
              -> FruitTart TemplateValue
tfShowInteger bindings parameters = do
  requireNParameters parameters 1 "showInteger"
  integer <- valueToInteger $ head parameters
  return $ TemplateString $ show integer


tfByteSizeToString :: Map (String, String) TemplateValue
                   -> [TemplateValue]
                   -> FruitTart TemplateValue
tfByteSizeToString bindings parameters = do
  requireNParameters parameters 1 "byteSizeToString"
  integer <- valueToInteger $ head parameters
  return $ TemplateString $ byteSizeToString integer


tfTimestampToString :: Map (String, String) TemplateValue
                    -> [TemplateValue]
                    -> FruitTart TemplateValue
tfTimestampToString bindings parameters = do
  requireNParameters parameters 1 "timestampToString"
  integer <- valueToInteger $ head parameters
  return $ TemplateString $ timestampToString integer


tfEscapeAttribute :: Map (String, String) TemplateValue
                   -> [TemplateValue]
                   -> FruitTart TemplateValue
tfEscapeAttribute bindings parameters = do
  requireNParameters parameters 1 "escapeAttribute"
  string <- valueToString $ head parameters
  return $ TemplateString $ escapeAttribute string


tfEscapeHTML :: Map (String, String) TemplateValue
             -> [TemplateValue]
             -> FruitTart TemplateValue
tfEscapeHTML bindings parameters = do
  requireNParameters parameters 1 "escapeHTML"
  string <- valueToString $ head parameters
  return $ TemplateString $ escapeHTML string


tfNewlinesToParagraphs :: Map (String, String) TemplateValue
                       -> [TemplateValue]
                       -> FruitTart TemplateValue
tfNewlinesToParagraphs bindings parameters = do
  requireNParameters parameters 1 "newlinesToParagraphs"
  string <- valueToString $ head parameters
  return $ TemplateString $ newlinesToParagraphs string


requireNParameters :: [TemplateValue] -> Int -> String -> FruitTart ()
requireNParameters parameters n functionName = do
  if length parameters /= n
    then error $ "Invalid number of parameters to " ++ functionName ++ "()."
    else return ()
