module Main where
import Loop
import Types
import ReadGameContent
import System.Environment (getArgs)
import System.Console.ANSI

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> error "must supply a file to open"
    [arg] -> do
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


