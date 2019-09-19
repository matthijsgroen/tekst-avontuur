module Loop (gameLoop) where
import Types
import Display
import Text.Read (readMaybe)
import System.Console.ANSI.Types
import System.Console.ANSI

replace :: Eq a => [a] -> [a] -> [a] -> [a]
replace needle replacement haystack =
  case begins haystack needle of
    Just remains -> replacement ++ remains
    Nothing      -> case haystack of
                      []     -> []
                      x : xs -> x : replace needle replacement xs

begins :: Eq a => [a] -> [a] -> Maybe [a]
begins haystack [] = Just haystack
begins (x:xs) (y:ys) | x == y = begins xs ys
begins _ _ = Nothing

match :: Comparator -> Value -> Value -> Bool
match Equals = (==)
match NotEquals = (/=)
match GreaterThan = (>)
match LessThan = (<)

conditionMet :: GameState -> Condition -> Bool
conditionMet (GameState _ values) (Condition slot operator value) =
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
-- TODO: Add support for random

replaceNth :: Int -> a -> [a] -> [a]
replaceNth _ _ [] = []
replaceNth n newVal (x:xs)
  | n == 0 = newVal:xs
  | otherwise = x:replaceNth (n - 1) newVal xs

applyMutation :: GameState -> Mutation -> GameState
applyMutation (GameState name values) (Mutation slot operator value) =
  let currentValue = values !! slot
      newValue = mutate operator currentValue value
  in GameState name (replaceNth slot newValue values)

isDigit :: Char -> Bool
isDigit x | x >= '0' && x <= '9' = True
isDigit _ = False

interpolateGameState :: [Int] -> [Char] -> [Char]
interpolateGameState state (x1:x2:x3:xs)
  | x1 == '#' && (isDigit x2) && (isDigit x2) = do
    let slot = read [x2, x3] :: Int
    show (state !! slot) ++ (interpolateGameState state xs)
interpolateGameState state (x:xs) = x:(interpolateGameState state xs)
interpolateGameState _ [] = []

interpolateText :: GameState -> DisplayData -> DisplayData
interpolateText (GameState name values) (Text x) =
  let replacedName = replace "$n" name x
      replacedState = interpolateGameState values replacedName
  in Text replacedState
interpolateText _ x = x

applyAction :: Action -> GameState -> GameState
applyAction (Action _ _ _ _ mutations) state =
  foldl applyMutation state mutations

applyDescription :: Description -> GameState -> (GameState, Description)
applyDescription (Description conditions displayData mutations) state =
  let newGameState = foldl applyMutation state mutations
      newDisplayData = map (interpolateText state) displayData
  in (newGameState, Description conditions newDisplayData mutations)

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
      let (newGamestate, newDescription) = applyDescription description gameState
      let (latestGameState, matchingDescriptions) = applyDescriptions newGamestate descriptions
      (latestGameState, [newDescription] ++ matchingDescriptions)
    False -> applyDescriptions gameState descriptions
applyDescriptions gameState [] = (gameState, [])

gameLoop :: Content -> GameState -> IO ()
gameLoop content@(Content _ descriptions actions) gameState = do
  let (updatedGameState, matchingDescriptions) = applyDescriptions gameState descriptions
  mapM_ printDescription matchingDescriptions

  putStrLn ""
  setSGR [Reset]
  let matchingActions =
        filter (isApplicableAction updatedGameState) actions
  mapM_ (uncurry printAction) $ zip matchingActions [1..]

  if null matchingActions
    then return ()
    else do
      userResponse <- readInt (length matchingActions)

      let applicableAction = matchingActions !! userResponse
      let newGamestate = applyAction applicableAction updatedGameState
      clearScreen
      setCursorPosition 0 0
      setSGR [Reset]

      gameLoop content newGamestate

