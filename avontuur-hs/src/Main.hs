module Main where
import Loop
import Types
import ReadGameContent
import System.Environment (getArgs)
import System.Console.ANSI
import System.IO

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> error "must supply a file to open"
    [arg] -> do
      -- https://stackoverflow.com/a/27331215
      hSetBuffering stdout NoBuffering
      clearScreen
      setCursorPosition 0 0
      putStrLn "Hoi! Welkom bij Avontuur! Wat is je naam?"
      name <- getLine
      let initialGameState = GameState name $ take 100 (repeat 0)
      clearScreen
      setCursorPosition 0 0
      content <- readGame arg
      gameLoop content initialGameState
    _ -> error "too many arguments"


