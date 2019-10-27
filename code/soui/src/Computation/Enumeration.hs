module Computation.Enumeration(allPossibleAssignments) where

import Model.Variable(Variable(..), VariableType(..))
import Model.World(Value(..), ValueAssignment(..))

allPossibleAssignments :: Variable -> [ValueAssignment]
allPossibleAssignments var =
    let varName = name var
        varType = variableType var
        valuesForType = allValuesForType varType
    in
        map (\val -> ValueAssignment {variableName = varName, assignedValue = val}) valuesForType

allValuesForType :: VariableType -> [Value]
allValuesForType BooleanType = [BooleanValue True, BooleanValue False]
