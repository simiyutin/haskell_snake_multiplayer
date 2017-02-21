{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Main where
 
import Network.Socket
import System.IO
import System.Random
import Control.Monad
import Control.Monad.State
import Control.Concurrent

import Data.Char
import Data.List
import Data.Map.Strict
 
main :: IO ()
main = do
    sock <- socket AF_INET Stream 0    -- create socket
    setSocketOption sock ReuseAddr 1   -- make socket immediately reusable - eases debugging.
    bind sock (SockAddrInet 4242 iNADDR_ANY)   -- listen on TCP port 4242.
    listen sock 2                              -- set a max of 2 queued connections

    connection <- accept sock
    hdl <- handleConnection connection

    -- force telnet client into character mode
    -- Let's go fucking craazyyyy (c)
    -- IAC DO LINEMODE IAC WILL ECHO (@see https://tools.ietf.org/html/rfc854#page-14,
    -- http://users.cs.cf.ac.uk/Dave.Marshall/Internet/node141.html)
    hPutStrLn hdl $ (chr(255):chr(253):chr(34):chr(255):chr(251):chr(1):[])

    runGame hdl

handleConnection :: (Socket, SockAddr) -> IO Handle
handleConnection (sock, addr) = do

  hdl <- socketToHandle sock ReadWriteMode
  hSetBuffering hdl NoBuffering

  hSetBuffering stdin NoBuffering
  hSetEcho stdin False

  return hdl


type Key = Char

data Direction = L | R | U | D deriving Show
type Position = (Int, Int)
type Body = [Position]
type Fruit = Position
data Status = Alive | Dead
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
    state <- takeMVar mGameState
    updatedState <- updateDirection 2 x state
    putMVar mGameState updatedState

  -- main game loop
  forever $ makeGameStep mGameState hdl >> threadDelay 100000


updateDirection :: Id -> Key -> GameState -> IO GameState
updateDirection id key state = return $ modifySnake id (setDir key) state
  where
    setDir :: Key -> Snake -> Snake
    setDir key (d, s, b) = case key of
      'w' -> (D, s, b)
      'a' -> (L, s, b)
      's' -> (U, s, b)
      'd' -> (R, s, b)
      _   -> (d, s, b)


makeGameStep :: MVar GameState -> Handle -> IO ()
makeGameStep mGameState hdl = do 
  prevstate <- takeMVar mGameState

  state <- execStateT iterateState prevstate
  frame <- getFrame state
  renderFrame hdl frame

  putMVar mGameState state


iterateState :: StateT GameState IO ()
iterateState = let

    helper :: Id -> StateT GameState IO ()
    helper id = do
      state <- get
      let head = getNextHead $ snakeMap state ! id
      if elem head (fruitList state) then do
        let restricted = (getSnakesCoords state) ++ (fruitList state)
        fruit <- getFruit restricted
        put $ (deleteFruit head . addFruit fruit . modifySnake id (addHead head)) state
      else
        put $ (modifySnake id (addHead head) . modifySnake id popTail) state

    dropLast xs = reverse (drop 1 $ reverse xs)

    addHead head snake@(d, s, b) = (d, s, head:b)
    popTail snake@(d, s, b) = (d, s, reverse $ drop 1 (reverse b))
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


getFrame :: GameState -> IO String
getFrame state = return $ helper 80 24 ""
  where
    helper 0  0  frame = frame
    helper 0  y  frame = helper 80 (pred y) (chr(10):frame)
    helper 80 y  frame = helper (pred 80) y ('|':frame)
    helper 1  y  frame = helper 0 y ('|':frame)
    helper x  24 frame = helper (pred x) 24 ('-':frame)
    helper x  1  frame = helper (pred x) 0 ('-':frame)
    helper x  y  frame
      | elem (x, y) (getSnakesCoords state)      = helper (pred x) y $ ('*':frame)
      | elem (x, y) (fruitList state)            = helper (pred x) y $ ('O':frame)  
      | otherwise                                = helper (pred x) y $ (' ':frame)


renderFrame :: Handle -> String -> IO ()
renderFrame hdl frame = do
  clearScreens hdl
  putStr frame
  hPutStrLn hdl $ frame


clearScreens :: Handle -> IO ()
clearScreens hdl = do
  let clearCommand = "\027[2J"
  let resetCursorCommand = "\027[H"
  hPutStr hdl clearCommand
  hPutStr hdl resetCursorCommand
  putStr clearCommand 
  putStr resetCursorCommand



