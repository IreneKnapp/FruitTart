module Network.FruitTart.Base.Templates.Semantics (
                                                   getTemplate,
                                                   fillTemplate,
                                                   eval,
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

import Network.FruitTart.Base
import Network.FruitTart.Base.View.Templates
import Network.FruitTart.Base.Templates.Syntax
import Network.FruitTart.Base.Templates.Types
import Network.FruitTart.Util


getTemplate :: String -> String -> [TemplateValue a] -> FruitTart String
getTemplate moduleName templateName parameters = do
  oldBindings <- getBindings
  let newBindings = Map.fromList [(("Base", "parameters"),
                                   TemplateList parameters)]
      allBindings = Map.union newBindings oldBindings
  letBindings allBindings $ fillTemplate moduleName templateName


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
  mapM (\([SQLText kind, SQLText body], index) -> do
         case kind of
           "content" -> return body
           "expression" ->
               fCatch (do
                        FruitTartState { database = database } <- get
                        expression <- liftIO $ readExpression database
                                                              moduleName
                                                              body
                        (result, newBindings)
                          <- evalExpression TemplateNormalContext expression
                        setBindings newBindings
                        valueToStringAllowingNull result)
                      (\e -> error $ "While processing template "
                                   ++ moduleName ++ "."
                                   ++ templateName ++ ", item " ++ (show index)
                                   ++ ": " ++ (show (e :: SomeException)))
           _ -> error $ "Unknown template item type " ++ kind ++ ".")
       (zip items [1..])
       >>= return . concat


eval :: String -> String -> FruitTart (TemplateValue a)
eval moduleName body = do
  FruitTartState { database = database } <- get
  expression <- liftIO $ readExpression database moduleName body
  (result, _) <- evalExpression TemplateNormalContext expression
  return result


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


setBindings :: Map (String, String) TemplateValue -> FruitTart ()
setBindings newBindings = do
  bindingsMVar <- getInterfaceStateMVar "Base"
               :: FruitTart (MVar (Map (String, String) TemplateValue))
  liftIO $ swapMVar bindingsMVar newBindings
  return ()


valueToBoolean :: TemplateValue -> FruitTart Bool
valueToBoolean (TemplateBool boolean) = return boolean
valueToBoolean value = error $ "Template value is not a Boolean (it's " ++ (show value)
                             ++ ")."


valueToString :: TemplateValue -> FruitTart String
valueToString (TemplateString string) = return string
valueToString value = error $ "Template value is not a String (it's " ++ (show value)
                            ++ ")."

valueToStringAllowingNull :: TemplateValue -> FruitTart String
valueToStringAllowingNull (TemplateString string) = return string
valueToStringAllowingNull TemplateNull = return ""
valueToStringAllowingNull value = error $ "Template value is not a String or Null (it's "
                                        ++ (show value)
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


evalExpression :: TemplateContext
               -> TemplateExpression
               -> FruitTart (TemplateValue, Map (String, String) TemplateValue)
evalExpression context expression = do
    case expression of
      TemplateStringLiteral value -> returnWithUnchangedBindings value
      TemplateIntegerLiteral value -> returnWithUnchangedBindings value
      TemplateExpressionList subexpressions -> do
        values <- mapM (evalExpression context) subexpressions
                  >>= return . (map fst)
        returnWithUnchangedBindings $ TemplateList values
      TemplateOperationConcatenate aExpression bExpression -> do
        aValue <- evalExpression context aExpression >>= return . fst
        bValue <- evalExpression context bExpression >>= return . fst
        case (aValue, bValue) of
          (TemplateString aString, TemplateString bString)
              -> returnWithUnchangedBindings $ TemplateString $ aString ++ bString
          _ -> error "Cannot concatenate non-Strings."
      TemplateOperationEquals aExpression bExpression -> do
        aValue <- evalExpression context aExpression >>= return . fst
        bValue <- evalExpression context bExpression >>= return . fst
        templateEqual aValue bValue
      TemplateOperationNotEquals aExpression bExpression -> do
        aValue <- evalExpression context aExpression >>= return . fst
        bValue <- evalExpression context bExpression >>= return . fst
        result <- templateEqual aValue bValue >>= return . fst
        templateNegate result
      TemplateOperationNot expression -> do
        value <- evalExpression context expression >>= return . fst
        templateNegate value
      TemplateOperationAnd aExpression bExpression -> do
        aValue <- evalExpression context aExpression >>= return . fst
        bValue <- evalExpression context bExpression >>= return . fst
        templateAnd aValue bValue
      TemplateOperationOr aExpression bExpression -> do
        aValue <- evalExpression context aExpression >>= return . fst
        bValue <- evalExpression context bExpression >>= return . fst
        templateOr aValue bValue
      TemplateOperationGreaterEquals aExpression bExpression -> do
        aValue <- evalExpression context aExpression >>= return . fst
        bValue <- evalExpression context bExpression >>= return . fst
        result <- templateCompare aValue bValue
        returnWithUnchangedBindings $ TemplateBool $ case result of
                                                       EQ -> True
                                                       GT -> True
                                                       LT -> False
      TemplateOperationGreater aExpression bExpression -> do
        aValue <- evalExpression context aExpression >>= return . fst
        bValue <- evalExpression context bExpression >>= return . fst
        result <- templateCompare aValue bValue
        returnWithUnchangedBindings $ TemplateBool $ case result of
                                                       EQ -> False
                                                       GT -> True
                                                       LT -> False
      TemplateOperationLessEquals aExpression bExpression -> do
        aValue <- evalExpression context aExpression >>= return . fst
        bValue <- evalExpression context bExpression >>= return . fst
        result <- templateCompare aValue bValue
        returnWithUnchangedBindings $ TemplateBool $ case result of
                                                       EQ -> True
                                                       GT -> False
                                                       LT -> True
      TemplateOperationLess aExpression bExpression -> do
        aValue <- evalExpression context aExpression >>= return . fst
        bValue <- evalExpression context bExpression >>= return . fst
        result <- templateCompare aValue bValue
        returnWithUnchangedBindings $ TemplateBool $ case result of
                                                       EQ -> False
                                                       GT -> False
                                                       LT -> True
      TemplateOperationAdd aExpression bExpression -> do
        aValue <- evalExpression context aExpression >>= return . fst
        bValue <- evalExpression context bExpression >>= return . fst
        templateArithmetic aValue bValue (+)
      TemplateOperationSubtract aExpression bExpression -> do
        aValue <- evalExpression context aExpression >>= return . fst
        bValue <- evalExpression context bExpression >>= return . fst
        templateArithmetic aValue bValue (-)
      TemplateOperationMultiply aExpression bExpression -> do
        aValue <- evalExpression context aExpression >>= return . fst
        bValue <- evalExpression context bExpression >>= return . fst
        templateArithmetic aValue bValue (*)
      TemplateOperationDivide aExpression bExpression -> do
        aValue <- evalExpression context aExpression >>= return . fst
        bValue <- evalExpression context bExpression >>= return . fst
        templateArithmetic aValue bValue div
      TemplateIfExpression subexpressions -> do
        if length subexpressions /= 3
          then error $ "Invalid number of parameters to if()."
          else return ()
        let condition = head subexpressions
            ifTrue = head $ drop 1 subexpressions
            ifFalse = head $ drop 2 subexpressions
        result <- evalExpression context condition
                  >>= valueToBoolean . fst
        if result
          then evalExpression context ifTrue
          else evalExpression context ifFalse
      TemplateCaseExpression subexpressions -> do
        let n = length subexpressions
        if not ((n > 1) && (odd n))
          then error $ "Invalid number of parameters to case()."
          else return ()
        mainKey <- (evalExpression context $ head subexpressions)
                   >>= return . fst
        let case' items = do
              case items of
                [] -> error $ "No match in case() for " ++ (show mainKey) ++ "."
                (key:(value:rest)) -> do
                  case key of
                    TemplateVariable (_, "otherwise") ->
                      evalExpression context value
                    _ -> do
                      key <- evalExpression context key >>= return . fst
                      if mainKey == key
                        then evalExpression context value
                        else case' rest
        case' $ tail subexpressions
      TemplateCallExpression subexpressions -> do
        if length subexpressions < 1
           then error $ "Invalid number of parameters to call()."
           else return ()
        subparameters <- (mapM (evalExpression context) $ tail subexpressions)
                         >>= return . map fst
        oldBindings <- getBindings
        let newBindings = Map.fromList [(("Base", "parameters"),
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
        subparameters <- (mapM (evalExpression context) $ drop 2 subexpressions)
                         >>= return . map fst
        oldBindings <- getBindings
        let newBindings = Map.fromList [(("Base", "parameters"),
                                         TemplateList subparameters)]
            globalSubbindings = Map.union newBindings oldBindings
        (moduleName, templateName)
            <- case head subexpressions of
                 TemplateVariable result -> return result
                 _ -> error $ "First parameter to iterate() is not a variable."
        rows <- (evalExpression context $ head $ drop 1 subexpressions)
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
        parameters <- (mapM (evalExpression context) $ drop 1 subexpressions)
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
        theMap <- (evalExpression context $ head $ drop 1 subexpressions)
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
        function <- evalExpression context functionExpression
                    >>= return . fst
        actualParameters <- mapM (evalExpression context)
                                 actualParameterExpressions
                            >>= return . map fst
        result <- applyFunction context function actualParameters
        returnWithUnchangedBindings result
      TemplateLambdaExpression formalParameters body -> do
        bindings <- getBindings
        returnWithUnchangedBindings $ TemplateLambda formalParameters bindings body
      TemplateVariable variableName@(packageName, properName) -> do
        bindings <- getBindings
        case Map.lookup variableName bindings of
          Nothing -> case Map.lookup ("Base", properName) bindings of
                       Nothing -> do
                         maybeValue <- getTopLevelBinding variableName
                         case maybeValue of
                           Just value -> do
                             oldBindings <- getBindings
                             let newBindings
                                   = Map.union (Map.fromList
                                                     [(variableName, value)])
                                               oldBindings
                             return $ (value, newBindings)
                           Nothing -> error $ "Undefined variable "
                                              ++ packageName ++ "."
                                              ++ properName ++ "."
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
        value <- (evalExpression context $ head $ drop 1 subexpressions)
                 >>= return . fst
        oldBindings <- getBindings
        let newBindings = Map.union (Map.fromList [((moduleName, variableName),
                                                    value)])
                                    oldBindings
        return $ (TemplateNull, newBindings)
      TemplateBindMapExpression subexpressions -> do
        if length subexpressions /= 1
          then error $ "Invalid number of parameters to bindmap()."
          else return ()
        value <- (evalExpression context $ head subexpressions)
                 >>= return . fst
        passedMap <- valueToMap value
        oldBindings <- getBindings
        let newBindings = Map.union passedMap oldBindings
        return $ (TemplateNull, newBindings)
      TemplateSequence expressionA expressionB -> do
        (_, temporaryBindings) <- evalExpression context expressionA
        letBindings temporaryBindings $ evalExpression context expressionB


templateEqual :: TemplateValue
              -> TemplateValue
              -> FruitTart (TemplateValue, Map (String, String) TemplateValue)
templateEqual (TemplateBool a) (TemplateBool b)
    = returnWithUnchangedBindings $ TemplateBool $ a == b
templateEqual (TemplateInteger a) (TemplateInteger b)
    = returnWithUnchangedBindings $ TemplateBool $ a == b
templateEqual (TemplateCharacter a) (TemplateCharacter b)
    = returnWithUnchangedBindings $ TemplateBool $ a == b
templateEqual (TemplateString a) (TemplateString b)
    = returnWithUnchangedBindings $ TemplateBool $ a == b
templateEqual (TemplateOrdering a) (TemplateOrdering b)
    = returnWithUnchangedBindings $ TemplateBool $ a == b
templateEqual _ _
    = error $ "Template values in comparison are not the same type or "
            ++ "are not Booleans, Integers, Characters, Strings, or Orderings."


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
templateCompare (TemplateCharacter a) (TemplateCharacter b) = return $ compare a b
templateCompare _ _ = error $  "Template values in comparison are not the same type or "
                      ++ " are not Integers or Characters."


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


applyFunction :: TemplateContext
              -> TemplateValue
              -> [TemplateValue]
              -> FruitTart TemplateValue
applyFunction context function actualParameters = do
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


getTopLevelBinding :: (String, String) -> FruitTart (Maybe TemplateValue)
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


builtinBindings :: Map (String, String) (TemplateValue a)
builtinBindings = Map.fromList
               [(("Base", "parameters"), TemplateList []),
                (("Base", "Null"), TemplateNull),
                (("Base", "True"), TemplateBool True),
                (("Base", "False"), TemplateBool False),
                (("Base", "Nothing"), TemplateMaybe Nothing),
                (("Base", "Just"), TemplateNativeLambda tfJust),
                (("Base", "LT"), TemplateOrdering LT),
                (("Base", "GT"), TemplateOrdering GT),
                (("Base", "EQ"), TemplateOrdering EQ),
                (("Base", "parameter"), TemplateNativeLambda tfParameter),
                (("Base", "isNothing"), TemplateNativeLambda tfIsNothing),
                (("Base", "isJust"), TemplateNativeLambda tfIsJust),
                (("Base", "fromJust"), TemplateNativeLambda tfFromJust),
                (("Base", "stringWordCount"),
                 TemplateNativeLambda tfStringWordCount),
                (("Base", "groupBy"), TemplateNativeLambda tfGroupBy),
                (("Base", "mergeBy"), TemplateNativeLambda tfMergeBy),
                (("Base", "compareIntegers"),
                 TemplateNativeLambda tfCompareIntegers),
                (("Base", "showInteger"), TemplateNativeLambda tfShowInteger),
                (("Base", "showBool"), TemplateNativeLambda tfShowBool),
                (("Base", "byteSizeToString"),
                 TemplateNativeLambda tfByteSizeToString),
                (("Base", "timestampToString"),
                 TemplateNativeLambda tfTimestampToString),
                (("Base", "escapeAttribute"),
                 TemplateNativeLambda tfEscapeAttribute),
                (("Base", "escapeHTML"), TemplateNativeLambda tfEscapeHTML),
                (("Base", "newlinesToParagraphs"),
                 TemplateNativeLambda tfNewlinesToParagraphs),
                
                -- Everything below here is based on a corresponding function
                -- in Haskell, which may or may not have an identical name.
                
                -- Lists
                -- Lists - Basic functions
                (("Base", "head"), TemplateNativeLambda tfHead),
                (("Base", "last"), TemplateNativeLambda tfLast),
                (("Base", "tail"), TemplateNativeLambda tfTail),
                (("Base", "init"), TemplateNativeLambda tfInit),
                (("Base", "null"), TemplateNativeLambda tfNull),
                (("Base", "length"), TemplateNativeLambda tfLength),
                -- Lists - List transformations
                (("Base", "map"), TemplateNativeLambda tfMap),
                (("Base", "reverse"), TemplateNativeLambda tfReverse),
                (("Base", "intersperse"), TemplateNativeLambda tfIntersperse),
                (("Base", "intercalate"), TemplateNativeLambda tfIntercalate),
                (("Base", "transpose"), TemplateNativeLambda tfTranspose),
                (("Base", "subsequences"), TemplateNativeLambda tfSubsequences),
                (("Base", "permutations"), TemplateNativeLambda tfPermutations),
                -- Lists - Reducing lists (folds)
                (("Base", "foldl"), TemplateNativeLambda tfFoldl),
                (("Base", "foldl1"), TemplateNativeLambda tfFoldl1),
                (("Base", "foldr"), TemplateNativeLambda tfFoldr),
                (("Base", "foldr1"), TemplateNativeLambda tfFoldr1),
                -- Lists - Reducing lists (folds) - Special folds
                (("Base", "concat"), TemplateNativeLambda tfConcat),
                (("Base", "concatMap"), TemplateNativeLambda tfConcatMap),
                (("Base", "and"), TemplateNativeLambda tfAnd),
                (("Base", "or"), TemplateNativeLambda tfOr),
                (("Base", "any"), TemplateNativeLambda tfAny),
                (("Base", "all"), TemplateNativeLambda tfAll),
                (("Base", "sum"), TemplateNativeLambda tfSum),
                (("Base", "product"), TemplateNativeLambda tfProduct),
                (("Base", "maximum"), TemplateNativeLambda tfMaximum),
                (("Base", "minimum"), TemplateNativeLambda tfMinimum),
                -- Lists - Building lists
                -- Lists - Building lists - Scans
                (("Base", "scanl"), TemplateNativeLambda tfScanl),
                (("Base", "scanl1"), TemplateNativeLambda tfScanl1),
                (("Base", "scanr"), TemplateNativeLambda tfScanr),
                (("Base", "scanr1"), TemplateNativeLambda tfScanr1),
                -- Lists - Building lists - Accumulating maps
                (("Base", "mapAccumL"), TemplateNativeLambda tfMapAccumL),
                (("Base", "mapAccumR"), TemplateNativeLambda tfMapAccumR),
                -- Lists - Building lists - Replicate
                (("Base", "replicate"), TemplateNativeLambda tfReplicate),
                -- Lists - Building lists - Unfolding
                (("Base", "unfoldr"), TemplateNativeLambda tfUnfoldr),
                -- Lists - Sublists
                -- Lists - Sublists - Extracting sublists
                (("Base", "take"), TemplateNativeLambda tfTake),
                (("Base", "drop"), TemplateNativeLambda tfDrop),
                (("Base", "splitAt"), TemplateNativeLambda tfSplitAt),
                (("Base", "takeWhile"), TemplateNativeLambda tfTakeWhile),
                (("Base", "dropWhile"), TemplateNativeLambda tfDropWhile),
                (("Base", "span"), TemplateNativeLambda tfSpan),
                (("Base", "break"), TemplateNativeLambda tfBreak),
                (("Base", "stripPrefix"), TemplateNativeLambda tfStripPrefix),
                (("Base", "group"), TemplateNativeLambda tfGroup),
                (("Base", "inits"), TemplateNativeLambda tfInits),
                (("Base", "tails"), TemplateNativeLambda tfTails),
                -- Lists - Sublists - Predicates
                (("Base", "isPrefixOf"), TemplateNativeLambda tfIsPrefixOf),
                (("Base", "isSuffixOf"), TemplateNativeLambda tfIsSuffixOf),
                (("Base", "isInfixOf"), TemplateNativeLambda tfIsInfixOf),
                -- Lists - Searching lists
                -- Lists - Searching lists - Searching by equality
                (("Base", "elem"), TemplateNativeLambda tfElem),
                (("Base", "notElem"), TemplateNativeLambda tfNotElem),
                (("Base", "lookup"), TemplateNativeLambda tfLookup),
                -- Lists - Searching lists - Searching with a predicate
                (("Base", "find"), TemplateNativeLambda tfFind),
                (("Base", "filter"), TemplateNativeLambda tfFilter),
                (("Base", "partition"), TemplateNativeLambda tfPartition),
                -- Lists - Indexing lists
                (("Base", "nth"), TemplateNativeLambda tfNth),
                (("Base", "elemIndex"), TemplateNativeLambda tfElemIndex),
                (("Base", "elemIndices"), TemplateNativeLambda tfElemIndices),
                (("Base", "findIndex"), TemplateNativeLambda tfFindIndex),
                (("Base", "findIndices"), TemplateNativeLambda tfFindIndices),
                -- Lists - Special lists
                -- Lists - "Set" operations
                (("Base", "nub"), TemplateNativeLambda tfNub),
                (("Base", "delete"), TemplateNativeLambda tfDelete),
                (("Base", "deleteFirsts"), TemplateNativeLambda tfDeleteFirsts),
                (("Base", "union"), TemplateNativeLambda tfUnion),
                (("Base", "intersect"), TemplateNativeLambda tfIntersect),
                -- Lists - Ordered lists
                (("Base", "sort"), TemplateNativeLambda tfSort),
                (("Base", "insert"), TemplateNativeLambda tfInsert),
                -- Lists - Generalized functions
                (("Base", "nubBy"), TemplateNativeLambda tfNubBy),
                (("Base", "deleteBy"), TemplateNativeLambda tfDeleteBy),
                (("Base", "deleteFirstsBy"),
                 TemplateNativeLambda tfDeleteFirstsBy),
                (("Base", "unionBy"), TemplateNativeLambda tfUnionBy),
                (("Base", "intersectBy"), TemplateNativeLambda tfIntersectBy),
                (("Base", "groupBy"), TemplateNativeLambda tfGroupBy),
                (("Base", "sortBy"), TemplateNativeLambda tfSortBy),
                (("Base", "insertBy"), TemplateNativeLambda tfInsertBy),
                (("Base", "maximumBy"), TemplateNativeLambda tfMaximumBy),
                (("Base", "minimumBy"), TemplateNativeLambda tfMinimumBy),
                
                -- Strings
                -- Strings - Basic functions
                (("Base", "stringHead"), TemplateNativeLambda tfStringHead),
                (("Base", "stringLast"), TemplateNativeLambda tfStringLast),
                (("Base", "stringTail"), TemplateNativeLambda tfStringTail),
                (("Base", "stringInit"), TemplateNativeLambda tfStringInit),
                (("Base", "stringNull"), TemplateNativeLambda tfStringNull),
                (("Base", "stringLength"), TemplateNativeLambda tfStringLength),
                -- Strings - String transformations
                (("Base", "stringMap"), TemplateNativeLambda tfStringMap),
                (("Base", "stringReverse"),
                 TemplateNativeLambda tfStringReverse),
                (("Base", "stringIntersperse"),
                 TemplateNativeLambda tfStringIntersperse),
                (("Base", "stringIntercalate"),
                 TemplateNativeLambda tfStringIntercalate),
                (("Base", "stringTranspose"),
                 TemplateNativeLambda tfStringTranspose),
                (("Base", "stringSubsequences"),
                 TemplateNativeLambda tfStringSubsequences),
                (("Base", "stringPermutations"),
                 TemplateNativeLambda tfStringPermutations),
                -- Strings - Reducing strings (folds)
                (("Base", "stringFoldl"), TemplateNativeLambda tfStringFoldl),
                (("Base", "stringFoldl1"), TemplateNativeLambda tfStringFoldl1),
                (("Base", "stringFoldr"), TemplateNativeLambda tfStringFoldr),
                (("Base", "stringFoldr1"), TemplateNativeLambda tfStringFoldr1),
                -- Strings - Reducing strings (folds) - Special folds
                (("Base", "stringConcat"), TemplateNativeLambda tfStringConcat),
                (("Base", "stringConcatMap"),
                 TemplateNativeLambda tfStringConcatMap),
                (("Base", "stringAny"), TemplateNativeLambda tfStringAny),
                (("Base", "stringAll"), TemplateNativeLambda tfStringAll),
                -- Strings - Building strings
                -- Strings - Building strings - Scans
                (("Base", "stringScanl"), TemplateNativeLambda tfStringScanl),
                (("Base", "stringScanl1"), TemplateNativeLambda tfStringScanl1),
                (("Base", "stringScanr"), TemplateNativeLambda tfStringScanr),
                (("Base", "stringScanr1"), TemplateNativeLambda tfStringScanr1),
                -- Strings - Building strings - Accumulating maps
                (("Base", "stringMapAccumL"),
                 TemplateNativeLambda tfStringMapAccumL),
                (("Base", "stringMapAccumR"),
                 TemplateNativeLambda tfStringMapAccumR),
                -- Strings - Building strings - Replicate
                (("Base", "stringReplicate"),
                 TemplateNativeLambda tfStringReplicate),
                -- Strings - Building strings - Unfolding
                (("Base", "stringUnfoldr"),
                 TemplateNativeLambda tfStringUnfoldr),
                -- Strings - Substrings
                -- Strings - Substrings - Extracting sublists
                (("Base", "stringTake"), TemplateNativeLambda tfStringTake),
                (("Base", "stringDrop"), TemplateNativeLambda tfStringDrop),
                (("Base", "stringSplitAt"),
                 TemplateNativeLambda tfStringSplitAt),
                (("Base", "stringTakeWhile"),
                 TemplateNativeLambda tfStringTakeWhile),
                (("Base", "stringDropWhile"),
                 TemplateNativeLambda tfStringDropWhile),
                (("Base", "stringSpan"), TemplateNativeLambda tfStringSpan),
                (("Base", "stringBreak"), TemplateNativeLambda tfStringBreak),
                (("Base", "stringStripPrefix"),
                 TemplateNativeLambda tfStringStripPrefix),
                (("Base", "stringGroup"), TemplateNativeLambda tfStringGroup),
                (("Base", "stringInits"), TemplateNativeLambda tfStringInits),
                (("Base", "stringTails"), TemplateNativeLambda tfStringTails),
                -- Strings - Substrings - Predicates
                (("Base", "stringIsPrefixOf"),
                 TemplateNativeLambda tfStringIsPrefixOf),
                (("Base", "stringIsSuffixOf"),
                 TemplateNativeLambda tfStringIsSuffixOf),
                (("Base", "stringIsInfixOf"),
                 TemplateNativeLambda tfStringIsInfixOf),
                -- Strings - Searching strings
                -- Strings - Searching strings - Searching by equality
                (("Base", "stringElem"), TemplateNativeLambda tfStringElem),
                (("Base", "stringNotElem"),
                 TemplateNativeLambda tfStringNotElem),
                (("Base", "stringLookup"), TemplateNativeLambda tfStringLookup),
                -- Strings - Searching strings - Searching with a predicate
                (("Base", "stringFind"), TemplateNativeLambda tfStringFind),
                (("Base", "stringFilter"), TemplateNativeLambda tfStringFilter),
                (("Base", "stringPartition"),
                 TemplateNativeLambda tfStringPartition),
                -- Strings - Indexing strings
                (("Base", "stringNth"), TemplateNativeLambda tfStringNth),
                (("Base", "stringElemIndex"),
                 TemplateNativeLambda tfStringElemIndex),
                (("Base", "stringElemIndices"),
                 TemplateNativeLambda tfStringElemIndices),
                (("Base", "stringFindIndex"),
                 TemplateNativeLambda tfStringFindIndex),
                (("Base", "stringFindIndices"),
                 TemplateNativeLambda tfStringFindIndices),
                -- Strings - Text operations
                (("Base", "stringLines"), TemplateNativeLambda tfStringLines),
                (("Base", "stringWords"), TemplateNativeLambda tfStringWords),
                (("Base", "stringUnlines"),
                 TemplateNativeLambda tfStringUnlines),
                (("Base", "stringUnwords"),
                 TemplateNativeLambda tfStringUnwords),
               ]


tfJust :: TemplateContext
       -> [TemplateValue]
       -> FruitTart TemplateValue
tfJust context parameters = do
  requireNParameters parameters 1 "just"
  return $ TemplateMaybe $ Just $ head parameters


tfParameter :: TemplateContext
            -> [TemplateValue]
            -> FruitTart TemplateValue
tfParameter context parameters = do
  requireNParameters parameters 1 "parameters"
  n <- valueToInteger $ head parameters
  bindings <- getBindings
  parameters <- (return $ Map.lookup ("Base", "parameters") bindings)
                >>= valueToList . fromJust
  if n < (fromIntegral $ length parameters)
    then return $ head $ drop (fromIntegral n) parameters
    else error $ "Too few template parameters "
               ++ "for parameter(" ++ (show n) ++ ")."


tfIsNothing :: TemplateContext
            -> [TemplateValue]
            -> FruitTart TemplateValue
tfIsNothing context parameters = do
  requireNParameters parameters 1 "isNothing"
  value <- return $ head parameters
  return $ case value of
             TemplateMaybe Nothing -> TemplateBool True
             TemplateMaybe (Just _) -> TemplateBool False
             _ -> error $ "Parameter is not a Maybe in isNothing()."


tfIsJust :: TemplateContext
         -> [TemplateValue]
         -> FruitTart TemplateValue
tfIsJust context parameters = do
  requireNParameters parameters 1 "isJust"
  value <- return $ head parameters
  return $ case value of
             TemplateMaybe Nothing -> TemplateBool False
             TemplateMaybe (Just _) -> TemplateBool True
             _ -> error $ "Parameter is not a Maybe in isJust()."


tfFromJust :: TemplateContext
           -> [TemplateValue]
           -> FruitTart TemplateValue
tfFromJust context parameters = do
  requireNParameters parameters 1 "fromJust"
  value <- return $ head parameters
  return $ case value of
             TemplateMaybe Nothing
                 -> error $ "Parameter is nothing in fromJust()."
             TemplateMaybe (Just result) -> result
             _ -> error $ "Parameter is not a Maybe in fromJust()."


tfStringWordCount :: TemplateContext
                  -> [TemplateValue]
                  -> FruitTart TemplateValue
tfStringWordCount context parameters = do
  requireNParameters parameters 1 "stringWordCount"
  string <- valueToString $ head parameters
  let wordCount "" = 0
      wordCount string = case elemIndex ' ' string of
                           Nothing -> 1
                           Just index -> let (_, rest) = splitAt index string
                                         in 1 + wordCount (drop 1 rest)
  return $ TemplateInteger $ fromIntegral $ wordCount string


tfGroupBy :: TemplateContext
          -> [TemplateValue]
          -> FruitTart TemplateValue
tfGroupBy context parameters = do
  requireNParameters parameters 2 "mergeBy"
  let function = head parameters
  list <- valueToList $ head $ drop 1 parameters
  groupedLists <- groupByM (\a b -> do
                              TemplateBool bool
                                  <- applyFunction context function [a, b]
                              return bool)
                           list
  return $ TemplateList list


tfMergeBy :: TemplateContext
          -> [TemplateValue]
          -> FruitTart TemplateValue
tfMergeBy context parameters = do
  requireNParameters parameters 2 "mergeBy"
  let function = head parameters
  lists <- (valueToList $ head $ drop 1 parameters)
           >>= mapM valueToList
  mergedLists <- mergeByM (\a b -> do
                             TemplateOrdering ordering
                                 <- applyFunction context function [a, b]
                             return ordering)
                          lists
  return $ TemplateList mergedLists


tfCompareIntegers :: TemplateContext
                  -> [TemplateValue]
                  -> FruitTart TemplateValue
tfCompareIntegers context parameters = do
  requireNParameters parameters 2 "compareIntegers"
  a <- valueToInteger $ head parameters
  b <- valueToInteger $ head $ drop 1 parameters
  return $ TemplateOrdering $ compare a b


tfShowInteger :: TemplateContext
              -> [TemplateValue]
              -> FruitTart TemplateValue
tfShowInteger context parameters = do
  requireNParameters parameters 1 "showInteger"
  integer <- valueToInteger $ head parameters
  return $ TemplateString $ show integer


tfShowBool :: TemplateContext
           -> [TemplateValue]
           -> FruitTart TemplateValue
tfShowBool context parameters = do
  requireNParameters parameters 1 "showBool"
  bool <- valueToBoolean $ head parameters
  return $ TemplateString $ show bool


tfByteSizeToString :: TemplateContext
                   -> [TemplateValue]
                   -> FruitTart TemplateValue
tfByteSizeToString context parameters = do
  requireNParameters parameters 1 "byteSizeToString"
  integer <- valueToInteger $ head parameters
  return $ TemplateString $ byteSizeToString integer


tfTimestampToString :: TemplateContext
                    -> [TemplateValue]
                    -> FruitTart TemplateValue
tfTimestampToString context parameters = do
  requireNParameters parameters 1 "timestampToString"
  integer <- valueToInteger $ head parameters
  return $ TemplateString $ timestampToString integer


tfEscapeAttribute :: TemplateContext
                  -> [TemplateValue]
                  -> FruitTart TemplateValue
tfEscapeAttribute context parameters = do
  requireNParameters parameters 1 "escapeAttribute"
  string <- valueToString $ head parameters
  return $ TemplateString $ escapeAttribute string


tfEscapeHTML :: TemplateContext
             -> [TemplateValue]
             -> FruitTart TemplateValue
tfEscapeHTML context parameters = do
  requireNParameters parameters 1 "escapeHTML"
  string <- valueToString $ head parameters
  return $ TemplateString $ escapeHTML string


tfNewlinesToParagraphs :: TemplateContext
                       -> [TemplateValue]
                       -> FruitTart TemplateValue
tfNewlinesToParagraphs context parameters = do
  requireNParameters parameters 1 "newlinesToParagraphs"
  string <- valueToString $ head parameters
  return $ TemplateString $ newlinesToParagraphs string


tfHead :: TemplateContext
       -> [TemplateValue]
       -> FruitTart TemplateValue
tfHead context parameters = do
  requireNParameters parameters 1 "head"
  list <- valueToList $ head parameters
  return $ head list


tfLast :: TemplateContext
       -> [TemplateValue]
       -> FruitTart TemplateValue
tfLast context parameters = do
  requireNParameters parameters 1 "last"
  list <- valueToList $ head parameters
  return $ last list


tfTail :: TemplateContext
       -> [TemplateValue]
       -> FruitTart TemplateValue
tfTail context parameters = do
  requireNParameters parameters 1 "tail"
  list <- valueToList $ head parameters
  return $ TemplateList $ tail list


tfInit :: TemplateContext
       -> [TemplateValue]
       -> FruitTart TemplateValue
tfInit context parameters = do
  requireNParameters parameters 1 "init"
  list <- valueToList $ head parameters
  return $ TemplateList $ init list


tfNull :: TemplateContext
       -> [TemplateValue]
       -> FruitTart TemplateValue
tfNull context parameters = do
  requireNParameters parameters 1 "null"
  list <- valueToList $ head parameters
  return $ TemplateBool $ null list


tfLength :: TemplateContext
         -> [TemplateValue]
         -> FruitTart TemplateValue
tfLength context parameters = do
  requireNParameters parameters 1 "length"
  list <- valueToList $ head parameters
  return $ TemplateInteger $ fromIntegral $ length list


tfMap :: TemplateContext
      -> [TemplateValue]
      -> FruitTart TemplateValue
tfMap context parameters = do
  requireNParameters parameters 2 "map"
  let function = head parameters
  list <- valueToList $ head $ drop 1 parameters
  list' <- mapM (\a -> applyFunction context function [a]) list
  return $ TemplateList list'


tfReverse :: TemplateContext
          -> [TemplateValue]
          -> FruitTart TemplateValue
tfReverse context parameters = do
  requireNParameters parameters 1 "reverse"
  list <- valueToList $ head parameters
  return $ TemplateList $ reverse list


tfIntersperse :: TemplateContext
              -> [TemplateValue]
              -> FruitTart TemplateValue
tfIntersperse context parameters = do
  -- TODO


tfIntercalate :: TemplateContext
              -> [TemplateValue]
              -> FruitTart TemplateValue
tfIntercalate context parameters = do
  -- TODO


tfTranspose :: TemplateContext
            -> [TemplateValue]
            -> FruitTart TemplateValue
tfTranspose context parameters = do
  -- TODO


tfSubsequences :: TemplateContext
               -> [TemplateValue]
               -> FruitTart TemplateValue
tfSubsequences context parameters = do
  -- TODO


tfPermutations :: TemplateContext
               -> [TemplateValue]
               -> FruitTart TemplateValue
tfPermutations context parameters = do
  -- TODO


tfFoldl :: TemplateContext
        -> [TemplateValue]
        -> FruitTart TemplateValue
tfFoldl context parameters = do
  -- TODO


tfFoldl1 :: TemplateContext
         -> [TemplateValue]
         -> FruitTart TemplateValue
tfFoldl1 context parameters = do
  -- TODO


tfFoldr :: TemplateContext
        -> [TemplateValue]
        -> FruitTart TemplateValue
tfFoldr context parameters = do
  -- TODO


tfFoldr1 :: TemplateContext
         -> [TemplateValue]
         -> FruitTart TemplateValue
tfFoldr1 context parameters = do
  -- TODO


tfConcat :: TemplateContext
         -> [TemplateValue]
         -> FruitTart TemplateValue
tfConcat context parameters = do
  requireNParameters parameters 1 "concat"
  list <- valueToList $ head parameters
  sublists <- mapM valueToList list
  return $ TemplateList $ concat sublists


tfConcatMap :: TemplateContext
            -> [TemplateValue]
            -> FruitTart TemplateValue
tfConcatMap context parameters = do
  -- TODO


tfAnd :: TemplateContext
      -> [TemplateValue]
      -> FruitTart TemplateValue
tfAnd context parameters = do
  requireNParameters parameters 1 "and"
  list <- valueToList $ head parameters
  list <- mapM valueToBoolean list
  return $ TemplateBoolean $ and list


tfOr :: TemplateContext
     -> [TemplateValue]
     -> FruitTart TemplateValue
tfOr context parameters = do
  requireNParameters parameters 1 "or"
  list <- valueToList $ head parameters
  list <- mapM valueToBoolean list
  return $ TemplateBoolean $ or list


tfAny :: TemplateContext
      -> [TemplateValue]
      -> FruitTart TemplateValue
tfAny context parameters = do
  -- TODO


tfAll :: TemplateContext
      -> [TemplateValue]
      -> FruitTart TemplateValue
tfAll context parameters = do
  -- TODO


tfSum :: TemplateContext
      -> [TemplateValue]
      -> FruitTart TemplateValue
tfSum context parameters = do
  requireNParameters parameters 1 "sum"
  list <- valueToList $ head parameters
  list <- mapM valueToInteger list
  return $ TemplateInteger $ sum list


tfProduct :: TemplateContext
          -> [TemplateValue]
          -> FruitTart TemplateValue
tfProduct context parameters = do
  requireNParameters parameters 1 "product"
  list <- valueToList $ head parameters
  list <- mapM valueToInteger list
  return $ TemplateInteger $ product list


tfMaximum :: TemplateContext
          -> [TemplateValue]
          -> FruitTart TemplateValue
tfMaximum context parameters = do
  requireNParameters parameters 1 "maximum"
  list <- valueToList 4 head parameters
  case head list of
    TemplateInteger _ -> do
      list <- mapM valueToInteger list
      return $ TemplateInteger $ maximum list
    TemplateCharacter _ -> do
      list <- mapM valueToCharacter list
      return $ TemplateCharacter $ maximum list
    _ -> do
      error $ "Template value is not a List of Integers or Characters."


tfMinimum :: TemplateContext
          -> [TemplateValue]
          -> FruitTart TemplateValue
tfMinimum context parameters = do
  requireNParameters parameters 1 "minimum"
  list <- valueToList 4 head parameters
  case head list of
    TemplateInteger _ -> do
      list <- mapM valueToInteger list
      return $ TemplateInteger $ minimum list
    TemplateCharacter _ -> do
      list <- mapM valueToCharacter list
      return $ TemplateCharacter $ minimum list
    _ -> do
      error $ "Template value is not a List of Integers or Characters."


tfScanl :: TemplateContext
        -> [TemplateValue]
        -> FruitTart TemplateValue
tfScanl context parameters = do
  -- TODO


tfScanl1 :: TemplateContext
         -> [TemplateValue]
         -> FruitTart TemplateValue
tfScanl1 context parameters = do
  -- TODO


tfScanr :: TemplateContext
        -> [TemplateValue]
        -> FruitTart TemplateValue
tfScanr context parameters = do
  -- TODO


tfScanr1 :: TemplateContext
         -> [TemplateValue]
         -> FruitTart TemplateValue
tfScanr1 context parameters = do
  -- TODO


tfMapAcccumL :: TemplateContext
             -> [TemplateValue]
             -> FruitTart TemplateValue
tfMapAccumL context parameters = do
  -- TODO


tfMapAccumR :: TemplateContext
            -> [TemplateValue]
            -> FruitTart TemplateValue
tfMapAccumR context parameters = do
  -- TODO


tfReplicate :: TemplateContext
            -> [TemplateValue]
            -> FruitTart TemplateValue
tfReplicate context parameters = do
  -- TODO


tfUnfoldr :: TemplateContext
          -> [TemplateValue]
          -> FruitTart TemplateValue
tfUnfoldr context parameters = do
  -- TODO


tfTake :: TemplateContext
       -> [TemplateValue]
       -> FruitTart TemplateValue
tfTake context parameters = do
  -- TODO


tfDrop :: TemplateContext
       -> [TemplateValue]
       -> FruitTart TemplateValue
tfDrop context parameters = do
  -- TODO


tfSplitAt :: TemplateContext
          -> [TemplateValue]
          -> FruitTart TemplateValue
tfSplitAt context parameters = do
  -- TODO


tfTakeWhile :: TemplateContext
            -> [TemplateValue]
            -> FruitTart TemplateValue
tfTakeWhile context parameters = do
  -- TODO


tfDropWhile :: TemplateContext
            -> [TemplateValue]
            -> FruitTart TemplateValue
tfDropWhile context parameters = do
  -- TODO


tfSpan :: TemplateContext
       -> [TemplateValue]
       -> FruitTart TemplateValue
tfSpan context parameters = do
  -- TODO


tfBreak :: TemplateContext
        -> [TemplateValue]
        -> FruitTart TemplateValue
tfBreak context parameters = do
  -- TODO


tfStripPrefix :: TemplateContext
              -> [TemplateValue]
              -> FruitTart TemplateValue
tfStripPrefix context parameters = do
  -- TODO


tfGroup :: TemplateContext
        -> [TemplateValue]
        -> FruitTart TemplateValue
tfGroup context parameters = do
  -- TODO


tfInits :: TemplateContext
        -> [TemplateValue]
        -> FruitTart TemplateValue
tfInits context parameters = do
  -- TODO


tfTails :: TemplateContext
        -> [TemplateValue]
        -> FruitTart TemplateValue
tfTails context parameters = do
  -- TODO


tfIsPrefixOf :: TemplateContext
             -> [TemplateValue]
             -> FruitTart TemplateValue
tfIsPrefixOf context parameters = do
  -- TODO


tfIsSuffixOf :: TemplateContext
             -> [TemplateValue]
             -> FruitTart TemplateValue
tfIsSuffixOf context parameters = do
  -- TODO


tfIsInfixOf :: TemplateContext
            -> [TemplateValue]
            -> FruitTart TemplateValue
tfIsInfixOf context parameters = do
  -- TODO


tfElem :: TemplateContext
       -> [TemplateValue]
       -> FruitTart TemplateValue
tfElem context parameters = do
  -- TODO


tfNotElem :: TemplateContext
          -> [TemplateValue]
          -> FruitTart TemplateValue
tfNotElem context parameters = do
  -- TODO


tfLookup :: TemplateContext
         -> [TemplateValue]
         -> FruitTart TemplateValue
tfLookup context parameters = do
  -- TODO


tfFind :: TemplateContext
       -> [TemplateValue]
       -> FruitTart TemplateValue
tfFind context parameters = do
  -- TODO


tfFilter :: TemplateContext
         -> [TemplateValue]
         -> FruitTart TemplateValue
tfFilter context parameters = do
  -- TODO


tfPartition :: TemplateContext
            -> [TemplateValue]
            -> FruitTart TemplateValue
tfPartition context parameters = do
  -- TODO


tfNth :: TemplateContext
      -> [TemplateValue]
      -> FruitTart TemplateValue
tfNth context parameters = do
  requireNParameters parameters 2 "nth"
  n <- valueToInteger $ head parameters
  list <- valueToList $ head $ drop 1 parameters
  return $ head $ drop (fromIntegral n) list


tfElemIndex :: TemplateContext
            -> [TemplateValue]
            -> FruitTart TemplateValue
tfElemIndex context parameters = do
  -- TODO


tfElemIndices :: TemplateContext
              -> [TemplateValue]
              -> FruitTart TemplateValue
tfElemIndices context parameters = do
  -- TODO


tfFindIndex :: TemplateContext
            -> [TemplateValue]
            -> FruitTart TemplateValue
tfFindIndex context parameters = do
  -- TODO


tfFindIndices :: TemplateContext
              -> [TemplateValue]
              -> FruitTart TemplateValue
tfFindIndices context parameters = do
  -- TODO


tfNub :: TemplateContext
      -> [TemplateValue]
      -> FruitTart TemplateValue
tfNub context parameters = do
  -- TODO


tfDelete :: TemplateContext
         -> [TemplateValue]
         -> FruitTart TemplateValue
tfDelete context parameters = do
  -- TODO


tfDeleteFirsts :: TemplateContext
               -> [TemplateValue]
               -> FruitTart TemplateValue
tfDeleteFirsts context parameters = do
  -- TODO


tfUnion :: TemplateContext
        -> [TemplateValue]
        -> FruitTart TemplateValue
tfUnion context parameters = do
  -- TODO


tfIntersect :: TemplateContext
            -> [TemplateValue]
            -> FruitTart TemplateValue
tfIntersect context parameters = do
  -- TODO


tfSort :: TemplateContext
       -> [TemplateValue]
       -> FruitTart TemplateValue
tfSort context parameters = do
  -- TODO


tfInsert :: TemplateContext
         -> [TemplateValue]
         -> FruitTart TemplateValue
tfInsert context parameters = do
  -- TODO


tfNubBy :: TemplateContext
        -> [TemplateValue]
        -> FruitTart TemplateValue
tfNubBy context parameters = do
  -- TODO


tfDeleteBy :: TemplateContext
           -> [TemplateValue]
           -> FruitTart TemplateValue
tfDeleteBy context parameters = do
  -- TODO


tfDeleteFirstsBy :: TemplateContext
                 -> [TemplateValue]
                 -> FruitTart TemplateValue
tfDeleteFirstsBy context parameters = do
  -- TODO


tfUnionBy :: TemplateContext
          -> [TemplateValue]
          -> FruitTart TemplateValue
tfUnionBy context parameters = do
  -- TODO


tfIntersectsBy :: TemplateContext
               -> [TemplateValue]
               -> FruitTart TemplateValue
tfIntersectsBy context parameters = do
  -- TODO


tfGroupBy :: TemplateContext
          -> [TemplateValue]
          -> FruitTart TemplateValue
tfGroupBy context parameters = do
  -- TODO


tfSortBy :: TemplateContext
         -> [TemplateValue]
         -> FruitTart TemplateValue
tfSortBy context parameters = do
  -- TODO


tfInsertBy :: TemplateContext
           -> [TemplateValue]
           -> FruitTart TemplateValue
tfInsertBy context parameters = do
  -- TODO


tfMaximumBy :: TemplateContext
            -> [TemplateValue]
            -> FruitTart TemplateValue
tfMaximumBy context parameters = do
  -- TODO


tfMinimumBy :: TemplateContext
            -> [TemplateValue]
            -> FruitTart TemplateValue
tfMinimumBy context parameters = do
  -- TODO


tfStringHead :: TemplateContext
             -> [TemplateValue]
             -> FruitTart TemplateValue
tfStringHead context parameters = do
  -- TODO


tfStringLast :: TemplateContext
             -> [TemplateValue]
             -> FruitTart TemplateValue
tfStringLast context parameters = do
  -- TODO


tfStringTail :: TemplateContext
             -> [TemplateValue]
             -> FruitTart TemplateValue
tfStringTail context parameters = do
  -- TODO


tfStringInit :: TemplateContext
             -> [TemplateValue]
             -> FruitTart TemplateValue
tfStringInit context parameters = do
  -- TODO


tfStringNull :: TemplateContext
             -> [TemplateValue]
             -> FruitTart TemplateValue
tfStringNull context parameters = do
  -- TODO


tfStringLength :: TemplateContext
               -> [TemplateValue]
               -> FruitTart TemplateValue
tfStringLength context parameters = do
  requireNParameters parameters 1 "stringLength"
  string <- valueToString $ head parameters
  return $ TemplateInteger $ fromIntegral $ length string


tfStringMap :: TemplateContext
            -> [TemplateValue]
            -> FruitTart TemplateValue
tfStringMap context parameters = do
  -- TODO


tfStringReverse :: TemplateContext
                -> [TemplateValue]
                -> FruitTart TemplateValue
tfStringReverse context parameters = do
  -- TODO


tfStringIntersperse :: TemplateContext
                    -> [TemplateValue]
                    -> FruitTart TemplateValue
tfStringIntersperse context parameters = do
  -- TODO


tfStringIntercalate :: TemplateContext
                    -> [TemplateValue]
                    -> FruitTart TemplateValue
tfStringIntercalate context parameters = do
  requireNParameters parameters 2 "intercalate"
  string <- valueToString $ head parameters
  list <- valueToList $ head $ drop 1 parameters
  strings <- mapM valueToString list
  return $ TemplateString $ intercalate string strings


tfStringTranspose :: TemplateContext
                  -> [TemplateValue]
                  -> FruitTart TemplateValue
tfStringTranspose context parameters = do
  -- TODO


tfStringSubsequences :: TemplateContext
                     -> [TemplateValue]
                     -> FruitTart TemplateValue
tfStringSubsequences context parameters = do
  -- TODO


tfStringPermutations :: TemplateContext
                     -> [TemplateValue]
                     -> FruitTart TemplateValue
tfStringPermutations context parameters = do
  -- TODO


tfStringFoldl :: TemplateContext
              -> [TemplateValue]
              -> FruitTart TemplateValue
tfStringFoldl context parameters = do
  -- TODO


tfStringFoldl1 :: TemplateContext
               -> [TemplateValue]
               -> FruitTart TemplateValue
tfStringFoldl1 context parameters = do
  -- TODO


tfStringFoldr :: TemplateContext
              -> [TemplateValue]
              -> FruitTart TemplateValue
tfStringFoldr context parameters = do
  -- TODO


tfStringFoldr1 :: TemplateContext
               -> [TemplateValue]
               -> FruitTart TemplateValue
tfStringFoldr1 context parameters = do
  -- TODO


tfStringConcat :: TemplateContext
               -> [TemplateValue]
               -> FruitTart TemplateValue
tfStringConcat context parameters = do
  -- TODO


tfStringConcatMap :: TemplateContext
                  -> [TemplateValue]
                  -> FruitTart TemplateValue
tfStringConcatMap context parameters = do
  -- TODO


tfStringAny :: TemplateContext
            -> [TemplateValue]
            -> FruitTart TemplateValue
tfStringAny context parameters = do
  -- TODO


tfStringAll :: TemplateContext
            -> [TemplateValue]
            -> FruitTart TemplateValue
tfStringAll context parameters = do
  -- TODO


tfStringScanl :: TemplateContext
              -> [TemplateValue]
              -> FruitTart TemplateValue
tfStringScanl context parameters = do
  -- TODO


tfStringScanl1 :: TemplateContext
               -> [TemplateValue]
               -> FruitTart TemplateValue
tfStringScanl1 context parameters = do
  -- TODO


tfStringScanr :: TemplateContext
              -> [TemplateValue]
              -> FruitTart TemplateValue
tfStringScanr context parameters = do
  -- TODO


tfStringScanr1 :: TemplateContext
               -> [TemplateValue]
               -> FruitTart TemplateValue
tfStringScanr1 context parameters = do
  -- TODO


tfStringMapAccumL :: TemplateContext
                  -> [TemplateValue]
                  -> FruitTart TemplateValue
tfStringMapAccumL context parameters = do
  -- TODO


tfStringMapAccumR :: TemplateContext
                  -> [TemplateValue]
                  -> FruitTart TemplateValue
tfStringMapAccumR context parameters = do
  -- TODO


tfStringReplicate :: TemplateContext
                  -> [TemplateValue]
                  -> FruitTart TemplateValue
tfStringReplicate context parameters = do
  -- TODO


tfStringUnfoldr :: TemplateContext
                -> [TemplateValue]
                -> FruitTart TemplateValue
tfStringUnfoldr context parameters = do
  -- TODO


tfStringTake :: TemplateContext
             -> [TemplateValue]
             -> FruitTart TemplateValue
tfStringTake context parameters = do
  -- TODO


tfStringDrop :: TemplateContext
             -> [TemplateValue]
             -> FruitTart TemplateValue
tfStringDrop context parameters = do
  -- TODO


tfStringSplitAt :: TemplateContext
                -> [TemplateValue]
                -> FruitTart TemplateValue
tfStringSplitAt context parameters = do
  -- TODO


tfStringTakeWhile :: TemplateContext
                  -> [TemplateValue]
                  -> FruitTart TemplateValue
tfStringTakeWhile context parameters = do
  -- TODO


tfStringDropWhile :: TemplateContext
                  -> [TemplateValue]
                  -> FruitTart TemplateValue
tfStringDropWhile context parameters = do
  -- TODO


tfStringSpan :: TemplateContext
             -> [TemplateValue]
             -> FruitTart TemplateValue
tfStringSpan context parameters = do
  -- TODO


tfStringBreak :: TemplateContext
              -> [TemplateValue]
              -> FruitTart TemplateValue
tfStringBreak context parameters = do
  -- TODO


tfStringStripPrefix :: TemplateContext
                    -> [TemplateValue]
                    -> FruitTart TemplateValue
tfStringStripPrefix context parameters = do
  -- TODO


tfStringGroup :: TemplateContext
              -> [TemplateValue]
              -> FruitTart TemplateValue
tfStringGroup context parameters = do
  -- TODO


tfStringInits :: TemplateContext
              -> [TemplateValue]
              -> FruitTart TemplateValue
tfStringInits context parameters = do
  -- TODO


tfStringTails :: TemplateContext
              -> [TemplateValue]
              -> FruitTart TemplateValue
tfStringTails context parameters = do
  -- TODO


tfStringIsPrefixOf :: TemplateContext
                   -> [TemplateValue]
                   -> FruitTart TemplateValue
tfStringIsPrefixOf context parameters = do
  -- TODO


tfStringIsSuffixOf :: TemplateContext
                   -> [TemplateValue]
                   -> FruitTart TemplateValue
tfStringIsSuffixOf context parameters = do
  -- TODO


tfStringIsInfixOf :: TemplateContext
                  -> [TemplateValue]
                  -> FruitTart TemplateValue
tfStringIsInfixOf context parameters = do
  -- TODO


tfStringElem :: TemplateContext
             -> [TemplateValue]
             -> FruitTart TemplateValue
tfStringElem context parameters = do
  -- TODO


tfStringNotElem :: TemplateContext
                -> [TemplateValue]
                -> FruitTart TemplateValue
tfStringNotElem context parameters = do
  -- TODO


tfStringLookup :: TemplateContext
               -> [TemplateValue]
               -> FruitTart TemplateValue
tfStringLookup context parameters = do
  -- TODO


tfStringFind :: TemplateContext
             -> [TemplateValue]
             -> FruitTart TemplateValue
tfStringFind context parameters = do
  -- TODO


tfStringFilter :: TemplateContext
               -> [TemplateValue]
               -> FruitTart TemplateValue
tfStringFilter context parameters = do
  -- TODO


tfStringPartition :: TemplateContext
                  -> [TemplateValue]
                  -> FruitTart TemplateValue
tfStringPartition context parameters = do
  -- TODO


tfStringNth :: TemplateContext
            -> [TemplateValue]
            -> FruitTart TemplateValue
tfStringNth context parameters = do
  -- TODO


tfStringElemIndex :: TemplateContext
                  -> [TemplateValue]
                  -> FruitTart TemplateValue
tfStringElemIndex context parameters = do
  -- TODO


tfStringElemIndices :: TemplateContext
                    -> [TemplateValue]
                    -> FruitTart TemplateValue
tfStringElemIndices context parameters = do
  -- TODO


tfStringFindIndex :: TemplateContext
                  -> [TemplateValue]
                  -> FruitTart TemplateValue
tfStringFindIndex context parameters = do
  -- TODO


tfStringFindIndices :: TemplateContext
                    -> [TemplateValue]
                    -> FruitTart TemplateValue
tfStringFindIndices context parameters = do
  -- TODO


tfStringLines :: TemplateContext
              -> [TemplateValue]
              -> FruitTart TemplateValue
tfStringLines context parameters = do
  -- TODO


tfStringWords :: TemplateContext
              -> [TemplateValue]
              -> FruitTart TemplateValue
tfStringWords context parameters = do
  -- TODO


tfStringUnlines :: TemplateContext
                -> [TemplateValue]
                -> FruitTart TemplateValue
tfStringUnlines context parameters = do
  -- TODO


tfStringUnwords :: TemplateContext
                -> [TemplateValue]
                -> FruitTart TemplateValue
tfStringUnwords context parameters = do
  -- TODO


requireNParameters :: [TemplateValue] -> Int -> String -> FruitTart ()
requireNParameters parameters n functionName = do
  if length parameters /= n
    then error $ "Invalid number of parameters to " ++ functionName ++ "()."
    else return ()
