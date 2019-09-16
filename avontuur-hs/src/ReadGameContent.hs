module ReadGameContent (readGame) where
import Types
import Text.Megaparsec hiding (ParseError)
import qualified Text.Megaparsec.Char.Lexer as L ( lexeme, skipLineComment, space )
import Text.Megaparsec.Char (digitChar, letterChar, lowerChar, upperChar)
import Data.Void

type Parser = Parsec Void String

char :: Char -> Parser Char
char = single

doubleQuote :: Parser Char
doubleQuote = char '"'

singleQuote :: Parser Char
singleQuote = char '\''

eol :: Parser Char
eol = char '\n'

isNewLine :: Char -> Bool
isNewLine '\n' = True
isNewLine _ = False

pLineComment :: Parser String
pLineComment = do
  singleQuote <?> "Line comment"
  comment <- takeWhile1P (Just "comment") (\c -> not (isNewLine c)) <?> "Line comment"
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
  result <- pQuoted (pCondition `sepBy1` char ';')
  return $ result

pFieldSeperation :: Parser ()
pFieldSeperation = do
  optional $ many $ char ' '
  ((:) <$> char ',' <*> many (char ' ')) <|> ("" <$ eol)
  return ()

pText :: Parser DisplayData
pText = do
  text <- pQuoted (
    ((:) <$> (satisfy (\x -> x /= '\"' && x /= '&')) <*> (many (satisfy (\x -> x /= '\"'))))
    )
  return $ Text text

pEmptyText :: Parser DisplayData
pEmptyText =
  doubleQuote *> (Text "" <$ doubleQuote)

pColor :: Parser DisplayData
pColor = do
  color <- pQuoted (char '*' *> char 'c' *> pNumber)
  return $ Color color

pDisplayData :: Parser DisplayData
pDisplayData = try pColor <|> pText <|> pEmptyText

pDescription :: Parser Description
pDescription = do
  optional (many (pLineComment <|> "" <$ eol))
  conditions <- pConditions
  pFieldSeperation
  displayData <- pDisplayData `sepBy` pFieldSeperation

  return $ Description conditions displayData []

adventure :: Parser Content
adventure = do
  descriptions <- many pDescription
  return $ Content [] descriptions []

readGame :: FilePath -> IO Content
readGame filePath = do
  contents <- readFile filePath
  {-let fakeContents = "' hello\n\n\"0=0;2!1\", \"Some text!\", \"\"\n\"Some more!\"\n\"&\""-}
  let fakeContents = "\"0=0\", \"Some text!\", \"*c14\", \"e&\""
  {-let fakeContents = "\"0=0\", \"&\""-}
  case parse adventure "" fakeContents of
    Right content -> return $ content
    Left message -> do
      print message
      return $ Content [] [] []

  {-return $ Content [] [-}
    {-Description [ Condition 0 Equals 0 ] [Text "Hello world"] [],-}
    {-Description [ Condition 0 Equals 1 ] [Text "Goodbye", Color 14, Text " Woohoo!"] []-}
    {-] [-}
    {-Action [ Condition 0 Equals 0 ] "Complete game" [ Mutation 0 Assign 1 ]-}
    {-]-}
