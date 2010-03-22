{-# LANGUAGE TypeSynonymInstances, DeriveDataTypeable #-}
module Network.FruitTart.Util.Types where

import Control.Concurrent
import Control.Exception
import Control.Monad.State
import qualified Data.ByteString as BS
import Data.Dynamic
import Data.Int
import Data.List
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe

import Database.SQLite3
import Network.FastCGI


data FruitTartState  = FruitTartState {
      database :: Database,
      formVariableMapMVar :: MVar (Map String String),
      interfacesMapMVar :: MVar (Map String Interface),
      interfaceStateMVarMap :: Map String Dynamic,
      sessionID :: Maybe Int64
    }

type FruitTart = StateT FruitTartState FastCGI
instance Typeable1 FruitTart where
    typeOf1 function = mkTyConApp (mkTyCon "FruitTart") []
instance Typeable a => Typeable (FruitTart a) where
    typeOf = typeOfDefault

instance MonadFastCGI FruitTart where
    getFastCGIState = lift getFastCGIState
    implementationThrowFastCGI exception = lift $ fThrow exception
    implementationCatchFastCGI action handler = do
      state <- get
      (result, state')
          <- lift $ fCatch (evalStateT (do
                                         result <- action
                                         state' <- get
                                         return (result, state'))
                                       state)
                           (\e -> evalStateT
                                    (do
                                      result <- handler $ fromJust $ fromException e
                                      state' <- get
                                      return (result, state'))
                                    state)
      put state'
      return result
    implementationBlockFastCGI action = do
      state <- get
      (result, state') <- lift $ fBlock (evalStateT (do
                                                      result <- action
                                                      state' <- get
                                                      return (result, state'))
                                                    state)
      put state'
      return result
    implementationUnblockFastCGI action = do
      state <- get
      (result, state') <- lift $ fUnblock (evalStateT (do
                                                        result <- action
                                                        state' <- get
                                                        return (result, state'))
                                                      state)
      put state'
      return result


type ActionTable = Map String
                       (Map String
                            ([ParameterType],
                             [ParameterType],
                             [(String, ParameterType)],
                             Dynamic))
type ControllerTable = Map String ActionTable

data ParameterType = IDParameter
                   | StringParameter
                   | EitherStringIDParameter
                     deriving (Eq)


data Interface = Interface {
      interfaceVersion :: Integer,
      interfaceDispatchTable :: ControllerTable,
      interfaceModuleName :: String,
      interfaceModuleVersion :: Int64,
      interfaceModuleSchemaVersion :: Int64,
      interfacePrerequisites :: [(String, Int64)],
      interfaceInitDatabase :: Database -> IO (),
      interfaceInitState :: IO Dynamic,
      interfaceInitRequest :: FruitTart ()
    } deriving (Typeable)


makeActionTable
    :: [(String,
         String,
         [ParameterType],
         [ParameterType],
         [(String, ParameterType)],
         Dynamic)]
    -> ActionTable
makeActionTable allActions
    = Map.fromList
      $ map (\actionsWithName@((name, _, _, _, _, _):_)
             -> (name,
                 Map.fromList
                 $ map (\(_,
                          method,
                          mandatoryParameters,
                          optionalParameters,
                          namedParameters,
                          function)
                        -> (method, (mandatoryParameters,
                                     optionalParameters,
                                     namedParameters,
                                     function)))
                       actionsWithName))
            $ groupBy (\(nameA, _, _, _, _, _) (nameB, _, _, _, _, _) -> nameA == nameB)
                      $ sortBy (\(nameA, methodA, _, _, _, _)
                                 (nameB, methodB, _, _, _, _)
                                -> let compareNames = compare nameA nameB
                                       compareMethods = compare methodA methodB
                                   in if compareNames == EQ
                                        then compareMethods
                                        else compareNames)
                               allActions


combineActionTables :: [(String, ActionTable)] -> ControllerTable
combineActionTables = Map.fromList
