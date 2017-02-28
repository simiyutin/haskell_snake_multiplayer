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

import Data.List
import Data.Map.Strict

import TelnetOutput

type Key = Char
data Direction = L | R | U | D deriving (Show , Eq)
type Body = [Position]
type Fruit = Position
data Status = Alive | Dead deriving Eq
type Id = Int

data Snake = Snake {direction :: Direction, status :: Status, body :: Body, handle :: Handle}
data GameState = GameState {snakeMap :: Map Id Snake, fruitList :: [Fruit], counter :: Id} 


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

    state <- takeMVar mGameState
    newState <- addNewPlayer state hdl
    id <- getLastId newState
    putMVar mGameState newState

    -- handle client input
    forkIO $ forever $ do
      dir <- hGetChar hdl
      state <- takeMVar mGameState
      newState <- updateDirection id dir state
      putMVar mGameState newState

    
addNewPlayer :: GameState -> Handle -> IO GameState
addNewPlayer state hdl = do
    newBody <- spawnBody state
    let snake = Snake {direction = L, status = Alive, body = newBody, handle = hdl}
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


modifySnake :: Id -> (Snake -> Snake) -> GameState -> GameState
modifySnake id f state = state {snakeMap = Data.Map.Strict.insert id (f $ snakeMap state ! id) (snakeMap state)}


getSnakesCoords :: GameState -> [Position]
getSnakesCoords state = Data.Map.Strict.foldr (\snake acc -> body snake ++ acc) [] $ snakeMap state
  

updateDirection :: Id -> Key -> GameState -> IO GameState
updateDirection id key state = return $ modifySnake id (setDir key) state
  where
    setDir :: Key -> Snake -> Snake
    setDir key snake
        | key == 'w' && d /= U = snake {direction = D}
        | key == 'a' && d /= R = snake {direction = L}
        | key == 's' && d /= D = snake {direction = U}
        | key == 'd' && d /= L = snake {direction = R}
        | otherwise = snake
      where
        d = direction snake


makeGameStep :: MVar GameState -> IO ()
makeGameStep mGameState = do 
  prevstate <- takeMVar mGameState

  state <- execStateT iterateState prevstate
  frame <- genFrame state
  showFrameToAll state frame

  putMVar mGameState state


showFrameToAll :: GameState -> Frame -> IO ()
showFrameToAll state frame = forEach (snakeMap state) (showFrameToSnake frame) 

    
showFrameToSnake :: Frame -> Snake -> IO ()
showFrameToSnake frame snake = showFrame (handle snake) frame


iterateState :: StateT GameState IO ()
iterateState = let

    iterateById :: Id -> StateT GameState IO ()
    iterateById id = do
      state <- get
      let snake = snakeMap state ! id
      unless (status snake == Dead) $ do
        let head = getNextHead snake
        if elem head (fruitList state) then do
          let restricted = (getSnakesCoords state) ++ (fruitList state)
          fruit <- liftIO $ getFreePosition restricted
          put $ (deleteFruit head . addFruit fruit . modifySnake id (addHead head)) state
        else do
          let restricted = (getSnakesCoords state) ++ (getFieldEdges)
          put $ modifySnake id (popTail . checkCollision restricted . addHead head) state


    checkCollision :: [Position] -> Snake -> Snake
    checkCollision restricted snake 
        | elem head restricted = snake {status = Dead} 
        | otherwise = snake
      where 
        head:xs = body snake

    addHead :: Position -> Snake -> Snake
    addHead head snake = snake {body = head : body snake} 

    popTail :: Snake -> Snake
    popTail snake | status snake == Dead = snake
                  | otherwise            = snake {body = dropLast $ body snake} where
      dropLast xs = reverse (drop 1 $ reverse xs)

    addFruit :: Fruit -> GameState -> GameState
    addFruit fruit state = state {fruitList = fruit : fruitList state}

    deleteFruit :: Fruit -> GameState -> GameState
    deleteFruit fruit state = state {fruitList = Data.List.delete fruit (fruitList state)}

    getNextHead :: Snake -> Position
    getNextHead snake = case direction snake of 
        R -> (x + 2, y)
        L -> (x - 2, y)
        U -> (x, y + 1)
        D -> (x, y - 1)
      where
        (x,y) = head $ body snake

  in do
    state <- get
    forEach (keys $ snakeMap state) iterateById


forEach :: (Monad m, Foldable f) => f a -> (a -> m r) -> m r
forEach container f = foldM (\acc x -> f x) undefined container


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
