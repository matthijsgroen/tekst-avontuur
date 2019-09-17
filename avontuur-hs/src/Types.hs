module Types where

type Slot = Int
type Value = Int

data Comparator = Equals | NotEquals | GreaterThan | LessThan
data MutationOperator = Assign | Add | Subtract | Random

data Condition = Condition Slot Comparator Value
data Mutation = Mutation Slot MutationOperator Value

data Action = Action [Condition] String [Mutation]
data DisplayData = Text String | Color Int | Delay Int
data Description = Description [Condition] [DisplayData] [Mutation]

type MetaData = (String, String)
data Content = Content [MetaData] [Description] [Action]
data GameState = GameState [Value]

