module Model.Model(
    WorldModel(..),
    emptyModel
) where

import Model.Variable

data WorldModel = WorldModel {
    variables :: [Variable]
} deriving (Eq)

emptyModel :: WorldModel
emptyModel = WorldModel {
    variables = []
}
