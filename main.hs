{-# LANGUAGE FlexibleContexts #-}

module Main where

import Control.Monad
import Control.Monad.State
import Control.Monad.Reader
import Text.Read (readEither)

data StateTree = StateTree
  { name :: String
  , age :: Int
  , passes :: Int
  }

showAge :: StateTree -> String
showAge = show . age

showPasses :: StateTree -> String
showPasses = show . passes

instance Show StateTree where
  show s = "Name: " ++ name s ++ "; Age: " ++ showAge s ++ "; Passes: " ++ showPasses s

initialState :: StateTree
initialState = StateTree "Carson" 25 0

type TreeState = StateT StateTree

data MenuChoice = ChangeName | ChangeAge | Quit | Unknown deriving (Eq)

instance Show MenuChoice where
  show ChangeName = "Change your name!"
  show ChangeAge = "Change your age!"
  show Quit = "Quit the app!"
  show Unknown = "Not a real choice!"

intToChoice :: Int -> MenuChoice
intToChoice n = case n of
  0 -> ChangeName
  1 -> ChangeAge
  2 -> Quit
  _ -> Unknown

-- Reader monad for the choice menu
getChoices :: Monad m => ReaderT Int m MenuChoice
getChoices = do
  choice <- ask
  return (intToChoice choice)

-- This is effectively the menu
-- Do we abstract this into another monad?
promptUserForChoice :: IO Int
promptUserForChoice = do
  putStrLn "Select a choice:"
  putStrLn $ "[0] -> " ++ show (intToChoice 0)
  putStrLn $ "[1] -> " ++ show (intToChoice 1)
  putStrLn $ "[2] -> " ++ show (intToChoice 2)
  choice <- getLine
  case (readEither choice :: Either String Int) of
    Right val -> return val
    Left _ -> return 3

main :: IO ()
main = mainLoop initialState

mainLoop :: StateTree -> IO ()
mainLoop state = do
  userChoice <- promptUserForChoice
  choice <- runReaderT getChoices userChoice
  print choice
  case choice of
    ChangeName -> do
      (name, newState) <- runStateT getNewNameAndAlterState state
      putStrLn $ "New state: " ++ show newState
      mainLoop newState
    ChangeAge -> do
      (age, newState) <- runStateT getNewAgeAndAlterState state
      putStrLn $ "New state: " ++ show newState
      mainLoop newState
    Quit -> do
      putStrLn "Quitting!"
      return ()
    _ -> mainLoop state

getNewNameAndAlterState :: TreeState IO ()
getNewNameAndAlterState = do
  liftIO $ putStrLn "Choose your name!"
  name <- liftIO getLine
  alterName name

getNewAgeAndAlterState :: TreeState IO ()
getNewAgeAndAlterState = do
  liftIO $ putStrLn "Choose your new age!"
  potentialAge <- liftIO getLine
  case (readEither potentialAge :: Either String Int) of
    Right val -> alterAge val
    Left err -> do
      liftIO $ putStrLn err
      getNewAgeAndAlterState

nameStateVal :: StateTree -> String
nameStateVal = name

changeNameState :: String -> StateTree -> StateTree
changeNameState name (StateTree n age passes) = StateTree name age passes

nextPassesState :: StateTree -> StateTree
nextPassesState (StateTree name age passes) = StateTree name age (succ passes)

alterName :: MonadState StateTree m => String -> m ()
alterName newName = do
  (StateTree _ age passes) <- get
  put (StateTree newName age (succ passes))

alterAge :: MonadState StateTree m => Int -> m ()
alterAge newAge = do
  (StateTree name _ passes) <- get
  put (StateTree name newAge (succ passes))