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


data Player = Me | NotMe deriving Show
type Key = Char

data Direction = L | R | U | D deriving Show
type Position = (Int, Int)
type Snake = [Position]
type Fruits = [Position]
type GameState = ((Direction, Snake), (Direction, Snake), Fruits)
  

runGame :: Handle -> IO ()
runGame hdl = do

  mGameState <- newMVar ((U, [(10, 10),(12, 10),(14, 10)]), (L, [(20, 10), (20, 9), (20, 8)]), [(60, 10)])

  -- handle local input
  forkIO $ forever $ do
    x <- getChar
    state <- takeMVar mGameState
    updatedState <- updateDirection Me x state
    putMVar mGameState updatedState
    
  -- handle client input
  forkIO $ forever $ do
    x <- hGetChar hdl
    state <- takeMVar mGameState
    updatedState <- updateDirection NotMe x state
    putMVar mGameState updatedState

  -- main game loop
  forever $ makeGameStep mGameState hdl >> threadDelay 100000



makeGameStep :: MVar GameState -> Handle -> IO ()
makeGameStep mGameState hdl = do 
  prevstate <- takeMVar mGameState

  state <- execStateT iterateState prevstate
  frame <- getFrame state
  clearScreens hdl
  putStr frame
  hPutStrLn hdl $ frame

  putMVar mGameState state

getFrame :: GameState -> IO String
getFrame ((dir1, shape1), (dir2, shape2), f) = return $ helper 80 24 ""
  where
    helper 0  0  frame = frame
    helper 0  y  frame = helper 80 (pred y) (chr(10):frame)
    helper 80 y  frame = helper (pred 80) y ('|':frame)
    helper 1  y  frame = helper 0 y ('|':frame)
    helper x  24 frame = helper (pred x) 24 ('-':frame)
    helper x  1  frame = helper (pred x) 0 ('-':frame)
    helper x  y  frame
      | elem (x, y) shape1 || elem (x, y) shape2 = helper (pred x) y $ ('*':frame)
      | elem (x, y) f                            = helper (pred x) y $ ('O':frame)  
      | otherwise                                = helper (pred x) y $ (' ':frame)

iterateState :: StateT GameState IO ()
iterateState = let
    helper head@(x, y) Me = do
      ((dir, shape), notme, f) <- get
      if elem (x, y) f then do
        fruit <- liftIO $ getFruit
        put ((dir, (head:shape)), notme, (fruit : delete head f))
      else
        put ((dir, (head:reverse (drop 1 $ reverse shape))), notme, f)

    helper head@(x, y) NotMe = do
      (me, (dir, shape), f) <- get
      if elem (x, y) f then do
        fruit <- liftIO $ getFruit
        put (me, (dir, (head:shape)), (fruit : delete head f))
      else
        put (me, (dir, (head:reverse (drop 1 $ reverse shape))), f)


    change (R, shape@((x, y):xs)) player = helper (x + 2, y) player
    change (L, shape@((x, y):xs)) player = helper (x - 2, y) player
    change (U, shape@((x, y):xs)) player = helper (x, y + 1) player
    change (D, shape@((x, y):xs)) player = helper (x, y - 1) player
  in do
    (me, notme, f) <- get
    change me Me
    change notme NotMe

getFruit :: IO Position
getFruit = do
  x <- randomRIO (1, 39)
  y <- randomRIO (1, 11)
  return (x * 2, y * 2)


updateDirection :: Player -> Key -> GameState -> IO GameState
updateDirection player key ((dir1, (pos1:xs)), (dir2, (pos2:xss)), f) = case player of 
    Me    -> return ((getdir dir1 key, (pos1:xs)), (dir2, (pos2:xss)), f)
    NotMe -> return ((dir1, (pos1:xs)), (getdir dir2 key, (pos2:xss)), f)
  where 
    getdir _ 'w'  = D
    getdir _ 'a'  = L
    getdir _ 's'  = U
    getdir _ 'd'  = R
    getdir prev _ = prev


clearScreens :: Handle -> IO ()
clearScreens hdl = do
  let clearCommand = "\027[2J"
  let resetCursorCommand = "\027[H"
  hPutStr hdl clearCommand
  hPutStr hdl resetCursorCommand
  putStr clearCommand 
  putStr resetCursorCommand



