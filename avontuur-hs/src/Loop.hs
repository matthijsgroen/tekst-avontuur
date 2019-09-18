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

readInt :: Int -> IO Int
readInt max = do
  input <- getLine
  case readMaybe input :: Maybe Int of
    Just i | i > 0 && i <= max -> return (i - 1)
    _ -> readInt max

gameLoop :: Content -> GameState -> IO ()
gameLoop content@(Content _ descriptions actions) gameState = do
  -- TODO: Display directly on matching, or collect but apply mutations on the fly
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
      userResponse <- readInt (length matchingActions)

      let applicableAction = matchingActions !! userResponse
      let newGamestate = applyAction applicableAction gameState
      clearScreen
      setCursorPosition 0 0

      gameLoop content newGamestate

