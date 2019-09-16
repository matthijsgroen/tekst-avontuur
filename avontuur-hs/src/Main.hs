module Main where
import Loop
import Types
import ReadGameContent
import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> error "must supply a file to open"
    [arg] -> do
      let initialGameState = GameState (take 100 (repeat 0))
      content <- readGame arg
      gameLoop content initialGameState
    _ -> error "too many arguments"


