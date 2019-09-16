module Types where

type Slot = Int
type Value = Int

data Operator = Equals | NotEquals | GreaterThan | LessThan
data MutationOperator = Assign | Add | Subtract | Random

data Condition = Condition Slot Operator Value
data Mutation = Mutation Slot MutationOperator Value

data Action = Action [Condition] String [Mutation]
data DisplayData = Text String | Color Int
data Description = Description [Condition] [DisplayData] [Mutation]

data Content = Content [Description] [Action]
data GameState = GameState [Value]

