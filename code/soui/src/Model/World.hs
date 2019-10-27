module Model.World(
    World(..),
    ValueAssignment(..),
    Value(..),
    emptyWorld
) where

data World = World {
    valueAssignments :: [ValueAssignment]
} deriving (Show, Eq)

data ValueAssignment = ValueAssignment {
    variableName :: String,
    assignedValue :: Value
} deriving (Show, Eq)

data Value = BooleanValue Bool
           deriving (Show, Eq)

emptyWorld :: World
emptyWorld = World {
    valueAssignments = []
}
