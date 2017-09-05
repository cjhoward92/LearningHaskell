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

changeNameState :: StateTree -> String -> StateTree
changeNameState (StateTree _ age passes) name = StateTree name age (succ passes)

changeAgeState :: StateTree -> Int -> StateTree
changeAgeState (StateTree name _ passes) age = StateTree name age (succ passes)

alterName :: MonadState StateTree m => String -> m ()
alterName newName = do
  st <- get
  put (changeNameState st newName)

alterAge :: MonadState StateTree m => Int -> m ()
alterAge newAge = do
  st <- get
  put (changeAgeState st newAge)

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

processStateAction :: TreeState IO () -> StateTree -> IO StateTree
processStateAction action state = do
  (val, newState) <- runStateT action state
  putStrLn $ "New state: " ++ show newState
  return newState

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

getUserMenuChoice :: IO MenuChoice
getUserMenuChoice = do
  userChoice <- promptUserForChoice
  choice <- runReaderT getChoices userChoice
  putStrLn $ "You chose: " ++ show choice
  return choice

main :: IO ()
main = mainLoop initialState

mainLoop :: StateTree -> IO ()
mainLoop state = getUserMenuChoice >>= processMenuChoice state

processMenuChoice :: StateTree -> MenuChoice -> IO ()
processMenuChoice state choice = case choice of
    ChangeName -> processStateAction getNewNameAndAlterState state >>= mainLoop
    ChangeAge -> processStateAction getNewAgeAndAlterState state >>= mainLoop
    Quit -> do
      putStrLn "Quitting!"
      return ()
    _ -> mainLoop state
