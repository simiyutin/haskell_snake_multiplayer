{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Main where
 
import Network.Socket

import System.IO
import System.Random
import Control.Monad
import Control.Monad.State
import Control.Concurrent

--import Data.Char
import Data.List
import Data.Map.Strict

import TelnetOutput
 
main :: IO ()
main = do
    sock <- socket AF_INET Stream 0    -- create socket
    setSocketOption sock ReuseAddr 1   -- make socket immediately reusable - eases debugging.
    bind sock (SockAddrInet 4242 iNADDR_ANY)   -- listen on TCP port 4242.
    listen sock 2                              -- set a max of 2 queued connections

    connection <- accept sock
    hdl <- handleConnection connection

    forceTelnetClientCharMode hdl

    runGame hdl

handleConnection :: (Socket, SockAddr) -> IO Handle
handleConnection (sock, addr) = do
    hdl <- socketToHandle sock ReadWriteMode
    hSetBuffering hdl NoBuffering
    -- Maybe :: notMonad (:D) hSetBuffering stdout NoBuffering too?

    hSetBuffering stdin NoBuffering
    hSetEcho stdin False

    return hdl


type Key = Char

data Direction = L | R | U | D deriving (Show , Eq)
type Body = [Position]
type Fruit = Position
data Status = Alive | Dead deriving Eq
type Snake = (Direction, Status, Body)
type Id = Int

data GameState = GameState {snakeMap :: Map Id Snake, fruitList :: [Fruit]}

addSnake = undefined
deleteSnake = undefined
modifySnake id f state = state {snakeMap = Data.Map.Strict.insert id (f $ snakeMap state ! id) (snakeMap state)}
getSnakesCoords :: GameState -> [Position]
getSnakesCoords state = Data.Map.Strict.foldr (\(d, s, b) acc -> b ++ acc) [] $ snakeMap state
  
runGame :: Handle -> IO ()
runGame hdl = do

  let newState = GameState {
    snakeMap = fromList [(1, (U, Alive, [(10, 10),(12, 10),(14, 10)])), (2, (L, Alive, [(20, 10), (20, 9), (20, 8)]))],
    fruitList = [(60, 10)]
  }

  mGameState <- newMVar newState

  -- handle local input
  forkIO $ forever $ do
    x <- getChar
    state <- takeMVar mGameState
    updatedState <- updateDirection 1 x state
    putMVar mGameState updatedState
    
  -- handle client input
  forkIO $ forever $ do
    x <- hGetChar hdl
    hPutStr hdl ("Key: " ++ [x])
    state <- takeMVar mGameState
    updatedState <- updateDirection 2 x state
    putMVar mGameState updatedState

  -- main game loop
  forever $ makeGameStep mGameState hdl >> threadDelay 300000


updateDirection :: Id -> Key -> GameState -> IO GameState
updateDirection id key state = return $ modifySnake id (setDir key) state
  where
    setDir :: Key -> Snake -> Snake
    setDir key (d, s, b)
      | key == 'w' && d /= U = (D, s, b)
      | key == 'a' && d /= R = (L, s, b)
      | key == 's' && d /= D = (U, s, b)
      | key == 'd' && d /= L = (R, s, b)
      | otherwise = (d, s, b)


makeGameStep :: MVar GameState -> Handle -> IO ()
makeGameStep mGameState hdl = do 
  prevstate <- takeMVar mGameState

  state <- execStateT iterateState prevstate
  frame <- genFrame state
  showFrame hdl frame

  putMVar mGameState state


iterateState :: StateT GameState IO ()
iterateState = let

    helper :: Id -> StateT GameState IO ()
    helper id = do
      state <- get
      let snake@(d, s, b) = snakeMap state ! id
      unless (s == Dead) $ do
        let head = getNextHead snake
        if elem head (fruitList state) then do
          let restricted = (getSnakesCoords state) ++ (fruitList state)
          fruit <- getFruit restricted
          put $ (deleteFruit head . addFruit fruit . modifySnake id (addHead head)) state
        else do
          let restricted = (getSnakesCoords state) ++ (getFieldEdges)
          put $ modifySnake id (popTail . checkCollision restricted . addHead head) state

    getFieldEdges = [(x,y) | x <- [0..80], y <- [1..24], x == 0 || x == 80 || y == 1 || y == 24]    

    checkCollision :: [Position] -> Snake -> Snake
    checkCollision restricted snake@(d, _, head:b) 
      | elem head restricted = (d, Dead, b)
      | otherwise = snake

    addHead head snake@(d, s, b) = (d, s, head:b)
    popTail snake@(d, Dead, b) = snake
    popTail snake@(d, s, b) = (d, s, dropLast b) where
      dropLast xs = reverse (drop 1 $ reverse xs)
    addFruit fruit state = state {fruitList = fruit : fruitList state}
    deleteFruit fruit state = state {fruitList = Data.List.delete fruit (fruitList state)}

    getNextHead :: Snake -> Position
    getNextHead (dir, _, (x, y):xs) = case dir of 
      R -> (x + 2, y)
      L -> (x - 2, y)
      U -> (x, y + 1)
      D -> (x, y - 1)

  in do
    state <- get
    foldM (\x id -> helper id) undefined $ keys (snakeMap state)


getFruit :: [Position] -> StateT GameState IO Position
getFruit restricted = do
  x <- liftIO $ randomRIO (1, 39)
  y <- liftIO $ randomRIO (1, 11)
  let fruit = (x * 2, y * 2)
  if elem fruit restricted then
    getFruit restricted
  else
    return fruit

instance FrameCoordState GameState where
    getSymbolAt :: GameState -> Position -> Maybe FrameSymbolsSeq
    getSymbolAt state pos
        | elem pos (getSnakesCoords state) = Just (FrameSymbol (getColorModifier Blue, '*'))
        | elem pos (fruitList state) = Just (FrameSymbol (getColorModifier Red, 'O'))
        | otherwise = Nothing