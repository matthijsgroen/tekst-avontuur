module Types where

type Slot = Int
type Value = Int
type PlayerName = [Char]

data Comparator = Equals | NotEquals | GreaterThan | LessThan
data MutationOperator = Assign | Add | Subtract | Random

data Condition = Condition Slot Comparator Value
data Mutation = Mutation Slot MutationOperator Value

type ActionKey = Char
data ActionColor = ActionColor Int
data Action = Action [Condition] (Maybe ActionKey) (Maybe ActionColor) String [Mutation]

data DisplayData = Text String | Color Int | Delay Int
data Description = Description [Condition] [DisplayData] [Mutation]

type MetaData = (String, String)
data Content = Content [MetaData] [Description] [Action]
data GameState = GameState PlayerName [Value]

