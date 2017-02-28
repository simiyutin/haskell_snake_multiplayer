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

    let newState = GameState {
      snakeMap = fromList [],
      fruitList = [(60, 10)],
      counter = 0
    } 

    mGameState <- newMVar newState

    forkIO $ forever $ connectionsLoop mGameState sock 

    runGame mGameState
    


runGame :: MVar GameState -> IO ()
runGame mGameState = forever $ makeGameStep mGameState >> threadDelay 300000
    


connectionsLoop :: MVar GameState -> Socket -> IO ThreadId
connectionsLoop mGameState sock = do
    connection <- accept sock
    hdl <- handleConnection connection

    putStrLn "player connected" 
    state <- takeMVar mGameState
    newState <- addNewPlayer state hdl
    id <- getLastId newState
    putMVar mGameState newState

    -- handle client input
    forkIO $ forever $ do
      dir <- hGetChar hdl
      hPutStr hdl ("Key: " ++ [dir])
      state <- takeMVar mGameState
      newState <- updateDirection id dir state
      putMVar mGameState newState

    
addNewPlayer :: GameState -> Handle -> IO GameState
addNewPlayer state hdl = do
    body <- spawnBody state
    let snake = (L, Alive, body, hdl)
    return $ state {snakeMap = Data.Map.Strict.insert (counter state) snake (snakeMap state), counter = counter state + 1}

spawnBody :: GameState -> IO [Position]
spawnBody state = let
    helper restricted = do 
      head@(x,y) <- getFreePosition restricted
      if elem (x + 1, y) restricted || elem (x + 2, y) restricted then do
        helper restricted
      else return [head, (x + 1, y), (x + 2, y)]
  in do
    let restricted = (getSnakesCoords state) ++ (fruitList state) ++ getFieldEdges 
    helper restricted

getLastId :: GameState -> IO Id
getLastId state = return $ counter state - 1 

 
  
handleConnection :: (Socket, SockAddr) -> IO Handle
handleConnection (sock, addr) = do
    hdl <- socketToHandle sock ReadWriteMode
    hSetBuffering hdl NoBuffering
    -- Maybe :: notMonad (:D) hSetBuffering stdout NoBuffering too?
    hSetBuffering stdin NoBuffering
    hSetEcho stdin False
    forceTelnetClientCharMode hdl

    return hdl


type Key = Char

data Direction = L | R | U | D deriving (Show , Eq)
type Body = [Position]
type Fruit = Position
data Status = Alive | Dead deriving Eq
type Snake = (Direction, Status, Body, Handle)
type Id = Int

data GameState = GameState {snakeMap :: Map Id Snake, fruitList :: [Fruit], counter :: Int}

addSnake = undefined
deleteSnake = undefined
modifySnake id f state = state {snakeMap = Data.Map.Strict.insert id (f $ snakeMap state ! id) (snakeMap state)}
getSnakesCoords :: GameState -> [Position]
getSnakesCoords state = Data.Map.Strict.foldr (\(d, s, b, h) acc -> b ++ acc) [] $ snakeMap state
  

updateDirection :: Id -> Key -> GameState -> IO GameState
updateDirection id key state = return $ modifySnake id (setDir key) state
  where
    setDir :: Key -> Snake -> Snake
    setDir key (d, s, b, h)
      | key == 'w' && d /= U = (D, s, b, h)
      | key == 'a' && d /= R = (L, s, b, h)
      | key == 's' && d /= D = (U, s, b, h)
      | key == 'd' && d /= L = (R, s, b, h)
      | otherwise = (d, s, b, h)


makeGameStep :: MVar GameState -> IO ()
makeGameStep mGameState = do 
  prevstate <- takeMVar mGameState

  state <- execStateT iterateState prevstate
  frame <- genFrame state
  showFrameToAll state frame

  putMVar mGameState state


showFrameToAll :: GameState -> Frame -> IO ()
showFrameToAll state frame = foldM (\x snake -> showFrameToSnake snake frame) undefined $ snakeMap state

    
showFrameToSnake :: Snake -> Frame -> IO ()
showFrameToSnake (_, _, _, hdl) frame = showFrame hdl frame


iterateState :: StateT GameState IO ()
iterateState = let

    helper :: Id -> StateT GameState IO ()
    helper id = do
      state <- get
      let snake@(d, s, b, h) = snakeMap state ! id
      unless (s == Dead) $ do
        let head = getNextHead snake
        if elem head (fruitList state) then do
          let restricted = (getSnakesCoords state) ++ (fruitList state)
          fruit <- liftIO $ getFreePosition restricted
          put $ (deleteFruit head . addFruit fruit . modifySnake id (addHead head)) state
        else do
          let restricted = (getSnakesCoords state) ++ (getFieldEdges)
          put $ modifySnake id (popTail . checkCollision restricted . addHead head) state


    checkCollision :: [Position] -> Snake -> Snake
    checkCollision restricted snake@(d, _, head:b, h) 
      | elem head restricted = (d, Dead, b, h)
      | otherwise = snake

    addHead head snake@(d, s, b, h) = (d, s, head:b, h)
    popTail snake@(d, Dead, b, h) = snake
    popTail snake@(d, s, b, h) = (d, s, dropLast b, h) where
      dropLast xs = reverse (drop 1 $ reverse xs)
    addFruit fruit state = state {fruitList = fruit : fruitList state}
    deleteFruit fruit state = state {fruitList = Data.List.delete fruit (fruitList state)}

    getNextHead :: Snake -> Position
    getNextHead (dir, _, (x, y):xs, _) = case dir of 
      R -> (x + 2, y)
      L -> (x - 2, y)
      U -> (x, y + 1)
      D -> (x, y - 1)

  in do
    state <- get
    foldM (\x id -> helper id) undefined $ keys (snakeMap state)

getFieldEdges :: [Position]
getFieldEdges = [(x,y) | x <- [0..80], y <- [1..24], x == 0 || x == 80 || y == 1 || y == 24]    

getFreePosition :: [Position] -> IO Position
getFreePosition restricted = do
  x <- randomRIO (1, 39)
  y <- randomRIO (1, 11)
  let fruit = (x * 2, y * 2)
  if elem fruit restricted then
    getFreePosition restricted
  else
    return fruit

instance FrameCoordState GameState where
    getSymbolAt :: GameState -> Position -> Maybe FrameSymbolsSeq
    getSymbolAt state pos
        | elem pos (getSnakesCoords state) = Just (FrameSymbol (getColorModifier Blue, '*'))
        | elem pos (fruitList state) = Just (FrameSymbol (getColorModifier Red, 'O'))
        | otherwise = Nothing
