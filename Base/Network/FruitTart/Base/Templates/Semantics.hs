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
               catchFruitTart ((evalExpression moduleName templateName
                                               bindings
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
valueToBoolean _ = error "Template value is not a Boolean."


valueToString :: TemplateValue -> FruitTart String
valueToString (TemplateString string) = return string
valueToString _ = error "Template value is not a String."


valueToInteger :: TemplateValue -> FruitTart Int64
valueToInteger (TemplateInteger integer) = return integer
valueToInteger _ = error "Template value is not an Integer."


valueToList :: TemplateValue -> FruitTart [TemplateValue]
valueToList (TemplateList list) = return list
valueToList _ = error "Template value is not a List."


valueToMap :: TemplateValue -> FruitTart (Map (String, String) TemplateValue)
valueToMap (TemplateMap map) = return map
valueToMap _ = error "Template value is not a Map."


evalExpression :: String
               -> String
               -> (Map (String, String) TemplateValue)
               -> TemplateExpression
               -> FruitTart TemplateValue
evalExpression moduleName templateName bindings expression = do
    case expression of
      TemplateLiteral value -> return value
      TemplateExpressionList subexpressions -> do
        values <- mapM (evalExpression moduleName templateName bindings)
                       subexpressions
        return $ TemplateList values
      TemplateOperationConcatenate aExpression bExpression -> do
        aValue <- evalExpression moduleName templateName bindings aExpression
        bValue <- evalExpression moduleName templateName bindings bExpression
        case (aValue, bValue) of
          (TemplateString aString, TemplateString bString)
              -> return $ TemplateString $ aString ++ bString
          _ -> error "Cannot concatenate non-Strings in template."
      TemplateOperationEquals aExpression bExpression -> do
        aValue <- evalExpression moduleName templateName bindings aExpression
        bValue <- evalExpression moduleName templateName bindings bExpression
        templateEqual aValue bValue
      TemplateOperationNotEquals aExpression bExpression -> do
        aValue <- evalExpression moduleName templateName bindings aExpression
        bValue <- evalExpression moduleName templateName bindings bExpression
        result <- templateEqual aValue bValue
        templateNegate result
      TemplateOperationNot expression -> do
        value <- evalExpression moduleName templateName bindings expression
        templateNegate value
      TemplateOperationAnd aExpression bExpression -> do
        aValue <- evalExpression moduleName templateName bindings aExpression
        bValue <- evalExpression moduleName templateName bindings bExpression
        templateAnd aValue bValue
      TemplateOperationOr aExpression bExpression -> do
        aValue <- evalExpression moduleName templateName bindings aExpression
        bValue <- evalExpression moduleName templateName bindings bExpression
        templateOr aValue bValue
      TemplateOperationGreaterEquals aExpression bExpression -> do
        aValue <- evalExpression moduleName templateName bindings aExpression
        bValue <- evalExpression moduleName templateName bindings bExpression
        result <- templateCompare aValue bValue
        return $ TemplateBool $ case result of
                                  EQ -> True
                                  GT -> True
                                  LT -> False
      TemplateOperationGreater aExpression bExpression -> do
        aValue <- evalExpression moduleName templateName bindings aExpression
        bValue <- evalExpression moduleName templateName bindings bExpression
        result <- templateCompare aValue bValue
        return $ TemplateBool $ case result of
                                  EQ -> False
                                  GT -> True
                                  LT -> False
      TemplateOperationLessEquals aExpression bExpression -> do
        aValue <- evalExpression moduleName templateName bindings aExpression
        bValue <- evalExpression moduleName templateName bindings bExpression
        result <- templateCompare aValue bValue
        return $ TemplateBool $ case result of
                                  EQ -> True
                                  GT -> False
                                  LT -> True
      TemplateOperationLess aExpression bExpression -> do
        aValue <- evalExpression moduleName templateName bindings aExpression
        bValue <- evalExpression moduleName templateName bindings bExpression
        result <- templateCompare aValue bValue
        return $ TemplateBool $ case result of
                                  EQ -> False
                                  GT -> False
                                  LT -> True
      TemplateOperationAdd aExpression bExpression -> do
        aValue <- evalExpression moduleName templateName bindings aExpression
        bValue <- evalExpression moduleName templateName bindings bExpression
        templateArithmetic aValue bValue (+)
      TemplateOperationSubtract aExpression bExpression -> do
        aValue <- evalExpression moduleName templateName bindings aExpression
        bValue <- evalExpression moduleName templateName bindings bExpression
        templateArithmetic aValue bValue (-)
      TemplateOperationMultiply aExpression bExpression -> do
        aValue <- evalExpression moduleName templateName bindings aExpression
        bValue <- evalExpression moduleName templateName bindings bExpression
        templateArithmetic aValue bValue (*)
      TemplateOperationDivide aExpression bExpression -> do
        aValue <- evalExpression moduleName templateName bindings aExpression
        bValue <- evalExpression moduleName templateName bindings bExpression
        templateArithmetic aValue bValue div
      TemplateIfExpression subexpressions -> do
        if length subexpressions /= 3
          then error $ "Invalid number of parameters to if() in template "
                     ++ moduleName ++ "." ++ templateName ++ "."
          else return ()
        let condition = head subexpressions
            ifTrue = head $ drop 1 subexpressions
            ifFalse = head $ drop 2 subexpressions
        result <- evalExpression moduleName templateName bindings condition
                  >>= valueToBoolean
        if result
          then evalExpression moduleName templateName bindings ifTrue
          else evalExpression moduleName templateName bindings ifFalse
      TemplateCaseExpression subexpressions -> do
        let n = length subexpressions
        if not ((n > 1) && (odd n))
          then error $ "Invalid number of parameters to case() in template "
                     ++ moduleName ++ "." ++ templateName ++ "."
          else return ()
        mainKey <- evalExpression moduleName templateName bindings
                                  $ head subexpressions
        let case' items = do
              case items of
                [] -> error $ "No match in case() in template "
                            ++ moduleName ++ "." ++ templateName ++ "."
                (key:(value:rest)) -> do
                  case key of
                    TemplateVariable (_, "otherwise") ->
                      evalExpression moduleName templateName bindings value
                    _ -> do
                      key <- evalExpression moduleName templateName bindings
                                            key
                      if mainKey == key
                        then evalExpression moduleName templateName bindings
                                            value
                        else case' rest
        case' $ tail subexpressions
      TemplateCallExpression subexpressions -> do
        if length subexpressions < 1
           then error $ "Invalid number of parameters to call() in template "
                      ++ moduleName ++ "." ++ templateName ++ "."
           else return ()
        subparameters <- mapM (evalExpression moduleName templateName bindings)
                              $ tail subexpressions
        let newBindings = Map.fromList [(("Templates", "parameters"),
                                         TemplateList subparameters)]
            subbindings = Map.union newBindings bindings
        (moduleName, templateName)
            <- case head subexpressions of
                 TemplateVariable result -> return result
                 _ -> error $ "First parameter is not a variable to call() in template "
                            ++ moduleName ++ "." ++ templateName ++ "."
        result <- fillTemplate moduleName templateName subbindings
        return $ TemplateString result
      TemplateIterateExpression subexpressions -> do
        if length subexpressions < 2
           then error $ "Invalid number of parameters to iterate() in template "
                      ++ moduleName ++ "." ++ templateName ++ "."
           else return ()
        subparameters <- mapM (evalExpression moduleName templateName bindings)
                              $ drop 2 subexpressions
        let newBindings = Map.fromList [(("Templates", "parameters"),
                                         TemplateList subparameters)]
            globalSubbindings = Map.union newBindings bindings
        (moduleName, templateName)
            <- case head subexpressions of
                 TemplateVariable result -> return result
                 _ -> error $ "First parameter is not a variable to iterate() "
                            ++ "in template " ++ moduleName ++ "." ++ templateName
                            ++ "."
        rows <- (evalExpression moduleName templateName bindings
                                $ head $ drop 1 subexpressions)
                >>= valueToList
        results <- mapM (\row -> do
                           row' <- valueToMap row
                           let localSubbindings = Map.union row' globalSubbindings
                           fillTemplate moduleName templateName
                                        localSubbindings)
                        rows
        return $ TemplateString $ concat results
      TemplateBoundExpression subexpressions -> do
        if length subexpressions /= 1
          then error $ "Invalid number of parameters to bound() in template "
                     ++ moduleName ++ "." ++ templateName ++ "."
          else return ()
        (moduleName, variableName)
            <- case head subexpressions of
                 TemplateVariable result -> return result
                 _ -> error $ "Parameter to bound() is not a variable."
        return $ case Map.lookup (moduleName, variableName) bindings of
             Nothing -> TemplateBool False
             Just _ -> TemplateBool True
      TemplateFunctionCall functionExpression actualParameterExpressions -> do
        function <- evalExpression moduleName templateName bindings functionExpression
        actualParameters <- mapM (\actualParameter ->
                                    evalExpression moduleName templateName
                                                   bindings actualParameter)
                                 actualParameterExpressions
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
            evalExpression moduleName templateName subbindings body
          TemplateNativeLambda body -> do
            body bindings actualParameters
          _ -> do
            error $ "Call to something not a function in template "
                  ++ moduleName ++ "." ++ templateName ++ "."
      TemplateLambdaExpression formalParameters body -> do
        return $ TemplateLambda formalParameters bindings body
      TemplateVariable variableName@(packageName, properName) -> do
        case Map.lookup variableName bindings of
          Nothing -> case Map.lookup ("Templates", properName) bindings of
                       Nothing -> error $ "Undefined variable " ++ packageName
                                        ++ "." ++ properName ++ "."
                       Just value -> return value
          Just value -> return value


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
