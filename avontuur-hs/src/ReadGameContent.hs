module ReadGameContent (readGame) where
import Types
import Text.Megaparsec
import Text.Megaparsec.Char (string, letterChar)
import Data.Void

type Parser = Parsec Void String

char :: Char -> Parser Char
char = single

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

pCompare :: Parser Comparator
pCompare =
  choice [ Equals <$ char '='
         , NotEquals <$ char '!'
         , GreaterThan <$ char '>'
         , LessThan <$ char '<'
         ]

pCondition :: Parser Condition
pCondition = do
  slot <- pNumber
  comparator <- pCompare
  value <- pNumber
  return $ Condition slot comparator value

pConditions :: Parser [Condition]
pConditions = do
  result <- pQuoted (pCondition `sepBy1` sep)
  return $ result

pActionKey :: Parser ActionKey
pActionKey = string "k=" *> letterChar

pActionColor :: Parser ActionColor
pActionColor = do
  string "c="
  colorCode <- pNumber
  return $ ActionColor colorCode

-- Action conditions can also hold an action color and key
pActionConditions :: Parser ([Condition], (Maybe ActionKey), (Maybe ActionColor))
pActionConditions = do
  doubleQuote
  conditions <- many $ try (optional sep *> pCondition)
  optional sep
  actionKey <- optional pActionKey
  optional sep
  actionColor <- optional pActionColor
  doubleQuote

  return $ (conditions, actionKey, actionColor)

pFieldSeperation :: Parser ()
pFieldSeperation = do
  optional $ many $ char ' '
  ((:) <$> char ',' <*> many (char ' ')) <|> (eol <* many (pLineComment <|> eol))
  return ()

pDisplayText :: Parser DisplayData
pDisplayText = do
  text <- pQuoted (
    ((:) <$> (satisfy (\x -> x /= '\"' && x /= '&')) <*> (many (satisfy (\x -> x /= '\"'))))
    )
  return $ Text text

pText :: Parser String
pText = do
  text <- pQuoted (some (satisfy (\x -> x /= '\"')))
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
pDisplayData = optional pFieldSeperation *> (
  try pColor <|>
  try pDelay <|>
  try pDisplayText <|>
  pEmptyText)

pOperator :: Parser MutationOperator
pOperator =
  choice [ Assign <$ char '='
         , Add <$ char '+'
         , Subtract <$ char '-'
         , Random <$ char 'r'
         ]

pMutation :: Parser Mutation
pMutation = do
  slot <- pNumber
  operator <- pOperator
  value <- pNumber
  return $ Mutation slot operator value

pDescriptionMutations :: Parser [Mutation]
pDescriptionMutations = do
  result <- pQuoted (char '&' *> (pMutation `sepBy` sep))
  return $ result

pDescription :: Parser Description
pDescription = do
  optional (many (pLineComment <|> eol))
  conditions <- pConditions
  displayData <- many (try pDisplayData)
  optional pFieldSeperation
  mutations <- pDescriptionMutations

  return $ Description conditions displayData mutations

pActionMutations :: Parser [Mutation]
pActionMutations = do
  result <- pQuoted (pMutation `sepBy` sep)
  return $ result

pAction :: Parser Action
pAction = do
  optional (many (pLineComment <|> eol))
  (conditions, actionKey, actionColor) <- pActionConditions
  optional pFieldSeperation
  actionText <- pText
  optional pFieldSeperation
  mutations <- pActionMutations
  return $ Action conditions actionKey actionColor actionText mutations

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
