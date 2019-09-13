module Main where
import Loop
import Types

main :: IO ()
main = do
  let initialGameState = GameState (take 100 (repeat 0))
  let content = Content [
        Description [ Condition 0 Equals 0 ] [Text "Hello world"] [],
        Description [ Condition 0 Equals 1 ] [Text "Goodbye", Color 14, Text " Woohoo!"] []
        ] [
        Action [ Condition 0 Equals 0 ] "Complete game" [ Mutation 0 Assign 1 ]
        ]

  gameLoop content initialGameState
