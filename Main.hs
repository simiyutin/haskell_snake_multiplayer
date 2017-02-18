module Main where
 
import Network.Socket
import System.IO
import System.Console.ANSI
import Control.Monad
import Control.Concurrent
 
main :: IO ()
main = do
    sock <- socket AF_INET Stream 0    -- create socket
    setSocketOption sock ReuseAddr 1   -- make socket immediately reusable - eases debugging.
    bind sock (SockAddrInet 4242 iNADDR_ANY)   -- listen on TCP port 4242.
    listen sock 2                              -- set a max of 2 queued connections

    connection <- accept sock
    hdl <- handleConnection connection

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
type GameState = ((Direction, Position), (Direction, Position)) -- todo Position -> Shape
  

runGame :: Handle -> IO ()
runGame hdl = do

  mGameState <- newMVar ((U, (0, 0)), (L, (50, 50)))

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

  forever $ do
    prevstate <- takeMVar mGameState
    state <- iterateState prevstate

    hPutStrLn hdl $ show state
    print state

    putMVar mGameState state

    threadDelay 100000
   
  return ()


iterateState :: GameState -> IO GameState
iterateState ((dir1, pos1), (dir2, pos2)) = let 
    change (x, y) R = (succ x, y)
    change (x, y) L = (pred x, y)
    change (x, y) U = (x, succ y)
    change (x, y) D = (x, pred y)
  in
    return ((dir1, change pos1 dir1), (dir2, change pos2 dir2))


updateDirection :: Player -> Key -> GameState -> IO GameState
updateDirection player key ((dir1, pos1), (dir2, pos2)) = case player of 
    Me    -> return ((getdir dir1 key, pos1), (dir2, pos2))
    NotMe -> return ((dir1, pos1), (getdir dir2 key, pos2))
  where 
    getdir _ 'w'  = U
    getdir _ 'a'  = L
    getdir _ 's'  = D
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



