module Main where
 
import Network.Socket
import System.IO
import System.Posix.Unistd
import System.Console.ANSI
import Control.Monad
import Control.Exception
import Control.Concurrent
import Control.Concurrent.Chan
import Control.Monad.Fix (fix)
 
main :: IO ()
main = do
    sock <- socket AF_INET Stream 0    -- create socket
    setSocketOption sock ReuseAddr 1   -- make socket immediately reusable - eases debugging.
    bind sock (SockAddrInet 4242 iNADDR_ANY)   -- listen on TCP port 4242.
    listen sock 2                              -- set a max of 2 queued connections
    chan <- newChan
    mainLoop sock chan


data Player = Me | NotMe deriving Show
type Key = Char
type Record = (Player, Key)

data Direction = Left | Right | Up | Down
type Position = (Int, Int)
type GameState = ((Direction, Position), (Direction, Position))

mainLoop :: Socket -> Chan Record -> IO ()
mainLoop sock chan = do
  connection <- accept sock
  hdl <- handleConnection connection chan
  runGame hdl chan


handleConnection :: (Socket, SockAddr) -> Chan Record -> IO Handle
handleConnection (sock, addr) chan = do
  hdl <- socketToHandle sock ReadWriteMode
  hSetBuffering hdl NoBuffering

  hSetBuffering stdin NoBuffering
  hSetEcho stdin False

  --пишем нажатия клавиш
  forkIO $ fix $ \loop -> do
    x <- getChar
    writeChan chan (Me , x) -- todo update state here 
    loop

  forkIO $ fix $ \loop -> do
    y <- hGetChar hdl
    writeChan chan (NotMe, y)  --todo update state here 
    loop

  return hdl

runGame hdl chan = do
  gameChan <- dupChan chan
  fix $ \loop -> do -- todo timer
    record <- readChan gameChan
    hPutStr hdl $ show record
    print record
    loop

  return ()




clearScreens hdl = do
  let clearCommand = "\027[2J"
  let resetCursorCommand = "\027[H"
  hPutStr hdl clearCommand
  hPutStr hdl resetCursorCommand
  putStr clearCommand 
  putStr resetCursorCommand



