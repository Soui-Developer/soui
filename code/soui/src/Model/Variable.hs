module Model.Variable(
    Variable(..),
    VariableType(..)
) where

data Variable = Variable {
    name :: String,
    variableType :: VariableType
} deriving (Eq)

data VariableType = BooleanType
    deriving (Eq)
