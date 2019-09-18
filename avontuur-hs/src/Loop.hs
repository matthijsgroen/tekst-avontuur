module Loop (gameLoop) where
import Types
import Display
import Text.Read (readMaybe)
import System.Console.ANSI.Types
import System.Console.ANSI

match :: Comparator -> Value -> Value -> Bool
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
isApplicableAction gameState (Action conditions _ _ _ _) =
  and (map (conditionMet gameState) conditions)

mutate :: MutationOperator -> Value -> Value -> Value
mutate Add a b = a + b
mutate Assign _ x = x
mutate Subtract a b = a - b

replaceNth :: Int -> a -> [a] -> [a]
replaceNth _ _ [] = []
replaceNth n newVal (x:xs)
  | n == 0 = newVal:xs
  | otherwise = x:replaceNth (n - 1) newVal xs

applyMutation :: GameState -> Mutation -> GameState
applyMutation (GameState values) (Mutation slot operator value) =
  let
    currentValue = values !! slot
    newValue = mutate operator currentValue value
  in
    GameState (replaceNth slot newValue values)

applyAction :: Action -> GameState -> GameState
applyAction (Action _ _ _ _ mutations) state =
  foldl applyMutation state mutations

applyDescription :: Description -> GameState -> GameState
applyDescription (Description _ _ mutations) state =
  foldl applyMutation state mutations

readInt :: Int -> IO Int
readInt max = do
  input <- getLine
  case readMaybe input :: Maybe Int of
    Just i | i > 0 && i <= max -> return (i - 1)
    _ -> readInt max

applyDescriptions :: GameState -> [Description] -> (GameState, [Description])
applyDescriptions gameState (description:descriptions) =
  case isApplicable gameState description of
    True -> do
      let newGamestate = applyDescription description gameState
      let (latestGameState, matchingDescriptions) = applyDescriptions newGamestate descriptions
      (latestGameState, [description] ++ matchingDescriptions)
    False -> applyDescriptions gameState descriptions
applyDescriptions gameState [] = (gameState, [])

gameLoop :: Content -> GameState -> IO ()
gameLoop content@(Content _ descriptions actions) gameState = do
  let (updatedGameState, matchingDescriptions) = applyDescriptions gameState descriptions
  mapM_ printDescription matchingDescriptions

  putStrLn ""
  let matchingActions =
        filter (isApplicableAction updatedGameState) actions
  mapM_ (uncurry printAction) (zip matchingActions [1..])

  if null matchingActions
    then return ()
    else do
      userResponse <- readInt (length matchingActions)

      let applicableAction = matchingActions !! userResponse
      let newGamestate = applyAction applicableAction updatedGameState
      clearScreen
      setCursorPosition 0 0

      gameLoop content newGamestate

