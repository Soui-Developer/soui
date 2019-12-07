module Model.Action where

import Model.World

data Action = Action {
    name :: ActionName,
    description :: String,
    effect :: World -> World
}

data ActionName = ActionName String
    deriving (Eq, Show)
