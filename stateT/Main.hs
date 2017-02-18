{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Main where
 
import Network.Socket
import System.IO
import System.Posix.Unistd
import System.Console.ANSI
import Control.Monad
import Control.Monad.State
import Control.Exception
import Control.Concurrent
import Control.Concurrent.Chan
import Control.Monad.Fix (fix)
 
main :: IO ()
main = do
    sock <- socket AF_INET Stream 0    -- create socket
    setSocketOption sock ReuseAddr 1   -- make socket immediately reusable - eases debugging.
    bind sock (SockAddrInet 4242 iNADDR_ANY)   -- listen on TCP port 4242.
    listen sock 2    
    connection <- accept sock
    
    hdl <- socketToHandle sock ReadWriteMode
    hSetBuffering hdl NoBuffering

    hSetBuffering stdin NoBuffering
    hSetEcho stdin False

    evalStateT (runGame hdl) ((U, (0, 0)), (L, (50, 50)))


data Player = Me | NotMe deriving Show
type Key = Char

data Direction = L | R | U | D deriving Show
type Position = (Int, Int)
type GameState = ((Direction, Position), (Direction, Position))


runGame :: Handle -> StateT GameState IO ()
runGame hdl = do

  

  --пишем нажатия клавиш
  
{--
  liftM forkIO $ fix $ \loop -> do
    x <- liftIO getChar
    liftIO $ putStrLn "me"
    --updateDirection Me x
    loop
--}

  

  liftM forkIO $ fix $ \loop -> do
    y <- liftIO $ hGetChar hdl
    liftIO $ putStrLn "notme"
    --updateDirection NotMe y
    loop

  

  fix $ \loop -> do -- todo timer
    state <- get
    liftIO $ print state
    --iteratePhysics
    --handleCollisions
    --render
    loop

  return ()

updateDirection :: Player -> Key -> StateT GameState IO ()

updateDirection Me    x = do
  ((direction, pos), notme) <- get
  put ((getnewdir direction x, pos), notme)


updateDirection NotMe x = do
  ((direction, pos), notme) <- get
  put ((getnewdir direction x, pos), notme)

getnewdir _ 'w' = U
getnewdir _ 'a' = L
getnewdir _ 's' = D
getnewdir _ 'd' = R
getnewdir prev _ = prev
  
iteratePhysics :: StateT GameState IO ()
iteratePhysics = undefined
handleCollisions = undefined
render = undefined


clearScreens hdl = do
  let clearCommand = "\027[2J"
  let resetCursorCommand = "\027[H"
  hPutStr hdl clearCommand
  hPutStr hdl resetCursorCommand
  putStr clearCommand 
  putStr resetCursorCommand



