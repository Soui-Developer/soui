import Test.Hspec

import Model.Model(WorldModel(..))
import Model.Variable(Variable(..), VariableType(..))
import Kernel(initialState, provideModel, provideWorld, think, bestWorldYetIdentified)
import Model.World(World(..), ValueAssignment(..), Value(..))

main :: IO ()
main = hspec $ do
    describe "Kernel" $ do
        it "supports Scenario 1 - Making a boolean be true" $ do
            let state0 = initialState
                model' = WorldModel {
                    variables = [
                        Variable {
                            name = "worldPeace",
                            variableType = BooleanType
                        }
                    ]
                }
                state1 = provideModel model' state0
                world' = World {
                    valueAssignments = [
                        ValueAssignment {variableName = "worldPeace", assignedValue = BooleanValue False}
                    ]
                }
                state2 = provideWorld world' state1
                state3 = think state2

            bestWorldYetIdentified state3 `shouldBe` (World {
                valueAssignments = [
                    ValueAssignment {variableName = "worldPeace", assignedValue = BooleanValue False}
                ]
            })
