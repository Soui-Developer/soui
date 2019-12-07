module Model.Message(Message(..)) where

import Model.World(World)
import Model.Action(ActionName)

data Message =
    -- | A request to provide the initial world model
    InitialModelRequest
    -- | A request to provide the initial state of the real world
    | InitialWorldRequest
    -- | A request to provide the initial utility function
    | InitialUtilityFunctionRequest
    -- | A request to add some action(s) which may help us move towards the given world state
    | ActionsTowardWorldRequest World
    -- | A request to perform a specific action
    | PerformActionRequest ActionName
    deriving (Show, Eq)
