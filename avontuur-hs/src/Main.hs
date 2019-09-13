module Main where

data Content = Content [Description] [Action]
data Description = Description [Condition] [DisplayData] [Mutation]
data Action = Action [Condition] String [Mutation]

data Condition = Condition Slot Operator Value
type Slot = Int
type Value = Int
data Operator = Equals | NotEquals | GreaterThan | LessThan
data DisplayData = Text String | Color Int
data Mutation = Mutation Slot MutationOperator Value
data MutationOperator = Assign | Add | Subtract | Random
data GameState = GameState [Value]

match :: Operator -> Value -> Value -> Bool
match Equals = (==)
match NotEquals = (/=)
match GreaterThan = (>)
match LessThan = (<)

conditionMet :: GameState -> Condition -> Bool
conditionMet (GameState values) (Condition slot operator value) =
  let stateValue = values !! slot
   in match operator stateValue value

isApplicable :: GameState -> Description -> Bool
isApplicable gameState (Description conditions _ _) =
  and (map (conditionMet gameState) conditions)

isApplicableAction :: GameState -> Action -> Bool
isApplicableAction gameState (Action conditions _ _) =
  and (map (conditionMet gameState) conditions)

printDisplayData :: DisplayData -> IO ()
printDisplayData (Text text) =
  putStr text

printDisplayData (Color code) =
  putStr ""

printDescription :: Description -> IO ()
printDescription (Description _ displayData _) = do
  mapM_ printDisplayData displayData
  putStrLn ""

printAction :: Action -> Int -> IO ()
printAction (Action _ text _) num = do
  putStrLn (show num ++ ") " ++ text)

applyAction :: Action -> GameState -> GameState
applyAction (Action _ _ mutations) state =
  {-Fold result here-}
  state

gameLoop :: Content -> GameState -> IO ()
gameLoop content@(Content descriptions actions) gameState = do
  let matchingDescriptions =
        filter (isApplicable gameState) descriptions
  mapM_ printDescription matchingDescriptions

  putStrLn ""
  let matchingActions =
        filter (isApplicableAction gameState) actions
  mapM_ (uncurry printAction) (zip matchingActions [1..])

  if null matchingActions
    then return ()
    else do
      let userResponse = 0
      let applicableAction = matchingActions !! userResponse
      let newGamestate = applyAction applicableAction gameState
      gameLoop content newGamestate

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
