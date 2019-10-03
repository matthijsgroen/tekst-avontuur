module ReadGameContent (readGame) where
import Types
import Text.Megaparsec
import Text.Megaparsec.Char (string, letterChar, char)
import Data.Void
import Data.Foldable

type Parser = Parsec Void String

doubleQuote :: Parser Char
doubleQuote = char '"'

singleQuote :: Parser Char
singleQuote = char '\''

sep :: Parser Char
sep = char ';'

eol :: Parser String
eol = "" <$ (optional (char '\r') *> char '\n')

isNewLine :: Char -> Bool
isNewLine '\n' = True
isNewLine '\r' = True
isNewLine _ = False

pLineComment :: Parser String
pLineComment = do
  singleQuote <?> "Line comment"
  comment <- takeWhileP (Just "comment") (\c -> not (isNewLine c)) <?> "Line comment"
  eol
  return $ comment

pQuoted :: Parser a -> Parser a
pQuoted p = doubleQuote *> p <* doubleQuote

pNumber :: Parser Int
pNumber = do
  number <- some (oneOf ['0'..'9']) <?> "Number"
  return $ (read number :: Int)

pCompare :: Ord a => Parser (a -> a -> Bool)
pCompare =
  choice [ (==) <$ char '='
         , (/=) <$ char '!'
         , (>) <$ char '>'
         , (<) <$ char '<'
         ]

slotValue :: GameState -> Slot -> Value
slotValue (GameState _ values) slot = values !! slot

createCondition :: (Value -> Value -> Bool) -> Slot -> Value -> Condition
createCondition compare slot value =
  Condition (\gameState -> compare (slotValue gameState slot) value)

pCondition :: Parser Condition
pCondition = do
  slot <- pNumber
  comparator <- pCompare
  value <- pNumber
  return $ createCondition comparator slot value

pConditions :: Parser Condition
pConditions = do
  result <- pQuoted $ pCondition `sepBy1` sep
  return $ fold result

pActionKey :: Parser ActionKey
pActionKey = string "k=" *> letterChar

pActionColor :: Parser ActionColor
pActionColor = do
  string "c="
  colorCode <- pNumber
  return $ ActionColor colorCode

-- Action conditions can also hold an action color and key
pActionConditions :: Parser (Condition, (Maybe ActionKey), (Maybe ActionColor))
pActionConditions = do
  doubleQuote
  conditions <- many $ try $ optional sep *> pCondition
  let condition = fold conditions

  optional sep
  actionKey <- optional pActionKey
  optional sep
  actionColor <- optional pActionColor
  doubleQuote

  return $ (condition, actionKey, actionColor)

pFieldSeperation :: Parser ()
pFieldSeperation = do
  optional $ many $ char ' '
  ((:) <$> char ',' <*> many (char ' ')) <|> (eol <* many (pLineComment <|> eol))
  return ()

noQuote :: Char -> Bool
noQuote x = x /= '\"'

noAmp :: Char -> Bool
noAmp x = x /= '&'

allP :: [a -> Bool] -> a -> Bool
allP (p:ps) a = (p a) && (allP ps a)
allP _ _ = True

pDisplayText :: Parser DisplayData
pDisplayText = do
  text <- pQuoted (
    ((:) <$> (satisfy (allP [noAmp, noQuote])) <*> (many $ satisfy noQuote))
    )
  return $ Text text

pText :: Parser String
pText = do
  text <- pQuoted (some $ satisfy noQuote)
  return $ text

pEmptyText :: Parser DisplayData
pEmptyText =
  doubleQuote *> (Text "" <$ doubleQuote)

pColor :: Parser DisplayData
pColor = do
  color <- pQuoted (char '*' *> char 'c' *> pNumber)
  return $ Color color

pDelay :: Parser DisplayData
pDelay = do
  delay <- pQuoted (char '*' *> char 's' *> pNumber)
  return $ Delay delay

pDisplayData :: Parser DisplayData
pDisplayData = optional pFieldSeperation *>
  ( try pColor <|>
    try pDelay <|>
    try pDisplayText <|>
    pEmptyText
  )

pOperator :: Parser MutationOperator
pOperator =
  choice [ Assign <$ char '='
         , Add <$ char '+'
         , Subtract <$ char '-'
         , Random 0 <$ char 'r'
         ]

replaceNth :: Int -> a -> [a] -> [a]
replaceNth _ _ [] = []
replaceNth n newVal (x:xs)
  | n == 0 = newVal:xs
  | otherwise = x:replaceNth (n - 1) newVal xs

mutate :: MutationOperator -> Value -> Value -> Value
mutate Add a b = a + b
mutate Assign _ x = x
mutate Subtract a b = a - b
-- TODO: Implement Random

createMutation :: Slot -> MutationOperator -> Value -> Mutation
createMutation slot operator value = Mutation (\(GameState name values) ->
    let currentValue = values !! slot
        newValue = mutate operator currentValue value
    in GameState name (replaceNth slot newValue values)
  )

pMutation :: Parser Mutation
pMutation = do
  slot <- pNumber
  operator <- pOperator
  value <- pNumber
  return $ createMutation slot operator value

pDescriptionMutations :: Parser Mutation
pDescriptionMutations = do
  result <- pQuoted (char '&' *> (pMutation `sepBy` sep))
  return $ fold result

pDescription :: Parser Description
pDescription = do
  optional (many (pLineComment <|> eol))
  condition <- pConditions
  displayData <- many (try pDisplayData)
  optional pFieldSeperation
  mutations <- pDescriptionMutations

  return $ Description condition displayData mutations

pActionMutations :: Parser Mutation
pActionMutations = do
  result <- pQuoted (pMutation `sepBy` sep)
  return $ fold result

pAction :: Parser Action
pAction = do
  optional (many (pLineComment <|> eol))
  (condition, actionKey, actionColor) <- pActionConditions
  optional pFieldSeperation
  actionText <- pText
  optional pFieldSeperation
  mutations <- pActionMutations
  return $ Action condition actionKey actionColor actionText mutations

adventure :: Parser Content
adventure = do
  descriptions <- many (try pDescription)
  optional (many (pLineComment <|> eol))
  pQuoted (string "END")
  actions <- many (try pAction)
  optional (many (pLineComment <|> eol))
  eof
  return $ Content [] descriptions actions

readGame :: FilePath -> IO Content
readGame filePath = do
  contents <- readFile filePath
  case parse adventure "" contents of
    Right content -> return $ content
    Left message -> do
      print message
      return $ Content [] [] []
