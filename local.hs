module Main where
 
import Network.Socket
import System.IO
import Control.Monad
 
ff :: Maybe Char -> Char
ff Nothing = ' '
ff (Just char) = char

main :: IO ()
main = do
  hSetBuffering stdin NoBuffering
  hSetEcho stdin False
  runGame

runGame = do 
  x <- getChar
  putStrLn ("You pressed: " ++ [x])
  runGame
