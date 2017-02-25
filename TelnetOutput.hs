{-# LANGUAGE FlexibleInstances #-}

module TelnetOutput where

-- format 4 spaces

import Data.Char
import System.IO

forceTelnetClientCharMode :: Handle -> IO ()
forceTelnetClientCharMode hdl = do
    -- force telnet client into character mode
    -- Let's go fucking craazyyyy (c)
    -- IAC DO LINEMODE IAC WILL ECHO (@see https://tools.ietf.org/html/rfc854#page-14,
    -- http://users.cs.cf.ac.uk/Dave.Marshall/Internet/node141.html)
    hPutStr hdl $ (chr(255):chr(253):chr(34):chr(255):chr(251):chr(1):[])

--renderFrame :: Handle -> String -> IO ()

-- for colors, etc
data Modifier = Modifier (String, String) | EmptyModifier
data FrameSymbolsSeq = FrameString (Modifier, String) | FrameSymbol (Modifier, Char)
data Frame = Frame [FrameSymbolsSeq]

infixr 5 @@

class FrameAppendable a where
    (@@) :: a -> Frame -> Frame

-- TODO use -XInstanceSigs to be more pedantic ;)
instance FrameAppendable FrameSymbolsSeq where
    --(@@) :: FrameSymbolsSeq -> Frame -> Frame
    s @@ (Frame xs) = Frame (s:xs)

instance FrameAppendable Char where
    --(@@) :: Char -> Frame -> Frame
    c @@ f = (FrameSymbol (EmptyModifier, c)) @@ f

instance FrameAppendable String where
    --(@@) :: String -> Frame -> Frame
    s @@ f = (FrameString (EmptyModifier, s)) @@ f

type Position = (Int, Int)
class FrameCoordState a where
    getSymbolAt :: a -> Position -> Maybe FrameSymbolsSeq

-- TODO modifiers support :D
instance Show Frame where
    show (Frame ((FrameString (_, s)):xs)) = s ++ show (Frame xs)
    show (Frame ((FrameSymbol (_, c)):xs)) = c:(show (Frame xs))
    show (Frame []) = ""

-- simple Frame generator
genFrame :: (FrameCoordState s) => s -> IO Frame
genFrame state = return $ helper 80 24 (Frame [])
    where
        helper :: Int -> Int -> Frame -> Frame
        helper 0  0  frame = frame
        helper 0  y  frame = helper 80 (pred y) (('\r':'\n':[]) @@ frame)--(chr(10):frame)
        helper 80 y  frame = helper (pred 80) y ('|' @@ frame)
        helper 1  y  frame = helper 0 y ('|' @@ frame)
        helper x  24 frame = helper (pred x) 24 ('-' @@ frame)
        helper x  1  frame = helper (pred x) 0 ('-' @@ frame)
        helper x  y  frame = helper (pred x) y $
            case (getSymbolAt state (x, y)) of
                 -- TODO colors!
                 -- we can show only separate symbols in that case ;)
                 Just (FrameSymbol (m, c)) -> (c @@ frame)
                 _ -> (' ' @@ frame)

showFrame :: Handle -> Frame -> IO ()
showFrame hdl frame = do
    showStrFrame hdl $ show frame

showStrFrame :: Handle -> String -> IO ()
showStrFrame hdl strFrame = do
    clearScreens hdl

    hPutStr stdout strFrame
    --hPutStr hdl $ "1\r\n2\r\n3"
    hPutStr hdl $ strFrame

clearScreen :: Handle -> IO ()
clearScreen hdl = do
    let clearDownCommand = chr(27):"[J"
    let clearCommand = chr(27):"[2J"
    let resetCursorCommand = chr(27):"[H"

    -- simply reset curson and rewrite all text... ;)
    hPutStr hdl resetCursorCommand

clearScreens :: Handle -> IO ()
clearScreens hdl = do
    clearScreen hdl
    clearScreen stdout