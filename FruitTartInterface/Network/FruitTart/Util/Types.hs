{-# LANGUAGE TypeSynonymInstances #-}
module Network.FruitTart.Util.Types where

import Control.Concurrent
import Control.OldException
import Control.Monad.State
import qualified Data.ByteString as BS
import Data.Int
import Data.List
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Dynamic
import Network.CGI
import Network.CGI.Monad

import Database.SQLite3


data FruitTartState  = FruitTartState {
      database :: Database,
      interfacesMapMVar :: MVar (Map String Interface),
      interfaceStateMVarMap :: Map String Dynamic,
      sessionID :: Maybe Int64
    }

type FruitTart = StateT FruitTartState (CGIT IO)
instance Typeable1 FruitTart where
    typeOf1 function = mkTyConApp (mkTyCon "FruitTart") []
instance Typeable a => Typeable (FruitTart a) where
    typeOf = typeOfDefault

liftCGI :: CGIT IO a -> FruitTart a
liftCGI = lift
instance MonadCGI FruitTart where
    cgiAddHeader name value = liftCGI $ cgiAddHeader name value
    cgiGet function = liftCGI $ cgiGet function

catchFruitTart :: (FruitTart a) -> (Exception -> FruitTart a) -> FruitTart a
catchFruitTart action handler = do
  state <- get
  (result, state') <- liftCGI $ catchCGI (evalStateT (do
                                                        result <- action
                                                        state' <- get
                                                        return (result, state'))
                                                     state)
                                         (\e -> (evalStateT (do
                                                               result <- handler e
                                                               state' <- get
                                                               return (result, state'))
                                                            state))
  put state'
  return result


type ActionTable = Map String
                       (Map String
                            ([ParameterType],
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
    }


makeActionTable
    :: [(String, String, [ParameterType], [(String, ParameterType)], Dynamic)]
    -> ActionTable
makeActionTable allActions
    = Map.fromList
      $ map (\actionsWithName@((name, _, _, _, _):_)
             -> (name,
                 Map.fromList
                 $ map (\(_, method, parameters, namedParameters, function)
                        -> (method, (parameters, namedParameters, function)))
                       actionsWithName))
            $ groupBy (\(nameA, _, _, _, _) (nameB, _, _, _, _) -> nameA == nameB)
                      $ sortBy (\(nameA, methodA, _, _, _) (nameB, methodB, _, _, _)
                                -> let compareNames = compare nameA nameB
                                       compareMethods = compare methodA methodB
                                   in if compareNames == EQ
                                        then compareMethods
                                        else compareNames)
                               allActions


combineActionTables :: [(String, ActionTable)] -> ControllerTable
combineActionTables = Map.fromList
