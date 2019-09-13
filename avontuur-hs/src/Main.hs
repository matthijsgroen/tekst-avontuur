module Main where

data Content = Content [Description] [Mutation]
data Description = Description [Condition] [DisplayData] [Mutation]

data Condition = Condition Slot Operator Value
type Slot = Int
type Value = Int
data Operator = Equals | NotEquals | GreaterThan | LessThan
data DisplayData = Text String | Markup Color
type Color = Int

data Mutation =
  Mutation Slot MutationOperator Value

data MutationOperator = Assign | Add | Subtract | Random
data GameState = GameState [Value]

gameLoop :: Content -> GameState -> IO ()
gameLoop = \x -> \y -> print "foo"

main :: IO ()
main = do
  let initialGameState = GameState (take 100 (repeat 0))
  let content = Content [
        Description [ Condition 0 Equals 0 ] [Text "Hello world"] []
        ] []

  gameLoop content initialGameState
