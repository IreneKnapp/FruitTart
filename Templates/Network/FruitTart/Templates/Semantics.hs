module Network.FruitTart.Templates.Semantics (
                                              fillTemplate
                                             )
    where

import Control.Exception
import Control.Monad.State
import Data.List
import Data.Map (Map)
import qualified Data.Map as Map
import Network.CGI.Monad
import Prelude hiding (catch)

import Network.FruitTart.Base
import Network.FruitTart.Templates.Syntax
import Network.FruitTart.Templates.Types
import Network.FruitTart.Util


fillTemplate :: String
             -> String
             -> (Map (String, String) TemplateValue)
             -> [TemplateValue]
             -> FruitTart String
fillTemplate moduleName templateName bindings parameters = do
  items <- query ("SELECT template_items.kind, template_items.body "
                  ++ "FROM templates LEFT JOIN template_items "
                  ++ "ON templates.id = template_items.template "
                  ++ "WHERE templates.module = ? AND templates.name = ? "
                  ++ "ORDER BY template_items.item")
                 [SQLText moduleName, SQLText templateName]
  if items == []
    then error $ "Template " ++ moduleName ++ "." ++ templateName ++ " not found."
    else return ()
  mapM (\[SQLText kindName, SQLText body] -> do
         let kind = case kindName of
                      "content" -> Content
                      "expression" -> Expression
                      _ -> Content
         case kind of
           Content -> return body
           Expression ->
               catchFruitTart ((evalExpression moduleName templateName
                                               bindings parameters
                                               $ readExpression moduleName body)
                               >>= return . valueToString)
                              (\e -> error $ "While parsing template "
                                           ++ moduleName ++ "."
                                           ++ templateName ++ ": " ++ (show e)))
       items
       >>= (\a -> do
              liftIO $ putStrLn $ "Filled template " ++ templateName ++ " getting " ++ (show a)
              return a)
       >>= return . concat


valueToBoolean :: TemplateValue -> Bool
valueToBoolean (TemplateBool boolean) = boolean
valueToBoolean _ = error "Template value is not a Boolean."


valueToString :: TemplateValue -> String
valueToString (TemplateString string) = string
valueToString _ = error "Template value is not a String."


valueToInteger :: TemplateValue -> Int64
valueToInteger (TemplateInteger integer) = integer
valueToInteger _ = error "Template value is not an Integer."


valueToList :: TemplateValue -> [TemplateValue]
valueToList (TemplateList list) = list
valueToList _ = error "Template value is not a List."


valueToMap :: TemplateValue -> Map (String, String) TemplateValue
valueToMap (TemplateMap map) = map
valueToMap _ = error "Template value is not a Map."


evalExpression :: String
               -> String
               -> (Map (String, String) TemplateValue)
               -> [TemplateValue]
               -> TemplateExpression
               -> FruitTart TemplateValue
evalExpression moduleName templateName bindings parameters expression =
    case expression of
      TemplateLiteral value -> return value
      TemplateExpressionList subexpressions -> do
        values <- mapM (evalExpression moduleName templateName bindings parameters)
                       subexpressions
        return $ TemplateList values
      TemplateOperationConcatenate aExpression bExpression -> do
        aValue <- evalExpression moduleName templateName bindings parameters aExpression
        bValue <- evalExpression moduleName templateName bindings parameters bExpression
        case (aValue, bValue) of
          (TemplateString aString, TemplateString bString)
              -> return $ TemplateString $ aString ++ bString
          _ -> error "Cannot concatenate non-Strings in template."
      TemplateFunctionCall (_, functionName) subexpressions
          -> handleFunctionCall moduleName templateName bindings parameters
                                functionName subexpressions
      TemplateVariable variableName@(packageName, properName)
          -> case Map.lookup variableName bindings of
               Nothing -> error $ "Undefined variable " ++ packageName ++ "."
                                ++ properName ++ " in template."
               Just value -> return value


handleFunctionCall
    :: String
    -> String
    -> (Map (String, String) TemplateValue)
    -> [TemplateValue]
    -> String
    -> [TemplateExpression]
    -> FruitTart TemplateValue
handleFunctionCall moduleName
                   templateName
                   bindings
                   parameters
                   functionName
                   subexpressions
    = let requireNParameters n =
              if length subexpressions /= n
                then error $ "Invalid number of parameters to "
                           ++ functionName
                           ++ "() in template "
                           ++ moduleName ++ "." ++ templateName ++ "."
                else return ()
      in case functionName of
        "if" -> do
          requireNParameters 3
          let condition = head subexpressions
              ifTrue = head $ drop 1 subexpressions
              ifFalse = head $ drop 2 subexpressions
          result <- evalExpression moduleName templateName bindings parameters condition
          if valueToBoolean result
            then evalExpression moduleName templateName bindings parameters ifTrue
            else evalExpression moduleName templateName bindings parameters ifFalse
        "case" -> do
          let n = length subexpressions
          if not ((n > 1) && (odd n))
            then error $ "Invalid number of parameters to case() in template "
                       ++ moduleName ++ "." ++ templateName ++ "."
            else return ()
          mainKey <- evalExpression moduleName templateName bindings parameters
                                    $ head subexpressions
          let case' items = do
                case items of
                  [] -> error $ "No match in case() in template "
                              ++ moduleName ++ "." ++ templateName ++ "."
                  (key:(value:rest)) -> do
                    case key of
                      TemplateVariable (_, "otherwise") ->
                        evalExpression moduleName templateName bindings parameters value
                      _ -> do
                        key <- evalExpression moduleName templateName bindings parameters
                                              key
                        if mainKey == key
                          then evalExpression moduleName templateName bindings parameters
                                              value
                          else case' rest
          case' $ tail subexpressions
        "call" -> do
          if length subexpressions < 1
             then error $ "Invalid number of parameters to call() in template "
                        ++ moduleName ++ "." ++ templateName ++ "."
             else return ()
          subparameters <- mapM (evalExpression moduleName templateName
                                                bindings parameters)
                                $ tail subexpressions
          (moduleName, templateName)
              <- case head subexpressions of
                   TemplateVariable result -> return result
                   _ -> error $ "First parameter is not a variable to call() in template "
                              ++ moduleName ++ "." ++ templateName ++ "."
          result <- fillTemplate moduleName templateName bindings subparameters
          return $ TemplateString result
        "iterate" -> do
          if length subexpressions < 2
             then error $ "Invalid number of parameters to iterate() in template "
                        ++ moduleName ++ "." ++ templateName ++ "."
             else return ()
          subparameters <- mapM (evalExpression moduleName templateName
                                                bindings parameters)
                                $ drop 2 subexpressions
          (moduleName, templateName)
              <- case head subexpressions of
                   TemplateVariable result -> return result
                   _ -> error $ "First parameter is not a variable to iterate() "
                              ++ "in template " ++ moduleName ++ "." ++ templateName
                              ++ "."
          rows <- (evalExpression moduleName templateName bindings parameters
                                  $ head $ drop 1 subexpressions)
                  >>= return . valueToList
          results <- mapM (\row -> do
                             let subbindings = valueToMap row
                             fillTemplate moduleName templateName
                                          subbindings subparameters)
                          rows
          return $ TemplateString $ concat results
        "iterateItems" -> do
          if length subexpressions < 2
             then error $ "Invalid number of parameters to iterateItems() in template "
                        ++ moduleName ++ "." ++ templateName ++ "."
             else return ()
          subparameters <- mapM (evalExpression moduleName templateName
                                                bindings parameters)
                                $ drop 2 subexpressions
          (moduleName, templateName)
              <- case head subexpressions of
                   TemplateVariable result -> return result
                   _ -> error $ "First parameter is not a variable to iterateItems() "
                              ++ "in template " ++ moduleName ++ "." ++ templateName
                              ++ "."
          rows <- (evalExpression moduleName templateName bindings parameters
                                  $ head $ drop 1 subexpressions)
                  >>= return . valueToList
          results <- mapM (\row -> do
                             let subbindings = Map.fromList [((moduleName, "item"),
                                                              row)]
                             fillTemplate moduleName templateName
                                          subbindings subparameters)
                          rows
          return $ TemplateString $ concat results
        "parameter" -> do
          requireNParameters 1
          n <- (evalExpression moduleName templateName bindings parameters
                               $ head subexpressions)
               >>= return . valueToInteger
          if n < (fromIntegral $ length parameters)
            then return $ head $ drop (fromIntegral n) parameters
            else error $ "Too few template parameters "
                       ++ "for parameter(" ++ (show n) ++ ") in template "
                       ++ moduleName ++ "." ++ templateName ++ "."
        "bound" -> do
          requireNParameters 1
          (moduleName, variableName)
              <- case head subexpressions of
                   TemplateVariable result -> return result
                   _ -> error $ "Parameter is not a variable to bound() in template "
                              ++ moduleName ++ "." ++ templateName ++ "."
          return $ case Map.lookup (moduleName, variableName) bindings of
            Nothing -> TemplateBool False
            Just _ -> TemplateBool True
        "nothing" -> do
          requireNParameters 0
          return $ TemplateMaybe Nothing
        "just" -> do
          requireNParameters 1
          result <- evalExpression moduleName templateName bindings parameters
                                   $ head subexpressions
          return $ TemplateMaybe $ Just result
        "isNothing" -> do
          requireNParameters 1
          value <- evalExpression moduleName templateName bindings parameters
                                  $ head subexpressions
          return $ case value of
            TemplateMaybe Nothing -> TemplateBool True
            TemplateMaybe (Just _) -> TemplateBool False
            _ -> error $ "Parameter is not a Maybe in isNothing() in template "
                       ++ moduleName ++ "." ++ templateName ++ "."
        "isJust" -> do
          requireNParameters 1
          value <- evalExpression moduleName templateName bindings parameters
                                  $ head subexpressions
          return $ case value of
            TemplateMaybe Nothing -> TemplateBool False
            TemplateMaybe (Just _) -> TemplateBool True
            _ -> error $ "Parameter is not a Maybe in isJust() in template "
                       ++ moduleName ++ "." ++ templateName ++ "."
        "fromJust" -> do
          requireNParameters 1
          value <- evalExpression moduleName templateName bindings parameters
                                  $ head subexpressions
          return $ case value of
            TemplateMaybe Nothing
                -> error $ "Parameter is nothing in fromJust() in template "
                         ++ moduleName ++ "." ++ templateName ++ "."
            TemplateMaybe (Just result) -> result
            _ -> error $ "Parameter is not a Maybe in fromJust() in template "
                       ++ moduleName ++ "." ++ templateName ++ "."
        "concat" -> do
          requireNParameters 1
          list <- (evalExpression moduleName templateName bindings parameters
                                  $ head subexpressions)
                  >>= return . valueToList
          return $ TemplateList $ concat $ map valueToList list
        "intercalate" -> do
          requireNParameters 2
          string <- (evalExpression moduleName templateName bindings parameters
                                    $ head subexpressions)
                    >>= return . valueToString
          list <- (evalExpression moduleName templateName bindings parameters
                                  $ head $ drop 1 subexpressions)
                  >>= return . valueToList
          return $ TemplateString $ intercalate string $ map valueToString list
        "showInteger" -> do
          requireNParameters 1
          integer <- (evalExpression moduleName templateName bindings parameters
                                     $ head subexpressions)
                     >>= return . valueToInteger
          return $ TemplateString $ show integer
        "byteSizeToString" -> do
          requireNParameters 1
          integer <- (evalExpression moduleName templateName bindings parameters
                                     $ head subexpressions)
                     >>= return . valueToInteger
          return $ TemplateString $ byteSizeToString integer
        "timestampToString" -> do
          requireNParameters 1
          integer <- (evalExpression moduleName templateName bindings parameters
                                     $ head subexpressions)
                     >>= return . valueToInteger
          return $ TemplateString $ timestampToString integer
        "escapeAttribute" -> do
          requireNParameters 1
          string <- (evalExpression moduleName templateName bindings parameters
                                    $ head subexpressions)
                    >>= return . valueToString
          return $ TemplateString $ escapeAttribute string
        "escapeHTML" -> do
          requireNParameters 1
          string <- (evalExpression moduleName templateName bindings parameters
                                    $ head subexpressions)
                    >>= return . valueToString
          return $ TemplateString $ escapeHTML string
        "newlinesToParagraphs" -> do
          requireNParameters 1
          string <- (evalExpression moduleName templateName bindings parameters
                                    $ head subexpressions)
                    >>= return . valueToString
          return $ TemplateString $ newlinesToParagraphs string
        _ -> do
          error $ "Invalid function name " ++ functionName ++ "() in template."
