import           Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Lib.EverythingManager
import Control.Monad.IO.Class

-- init_everything :: Property

-- Start with empty everything. add a todo. ensure todo is successfully added to everything
testAddToDo :: Property
testAddToDo =
  property $ do
  -- description <- forAll $ Gen.list (Range.linear 0 100) Gen.alpha
  -- priority <- forAll $ Gen.int (Range.linear 0 100) 
  let
    description = "TEST"
    position = 0
    everything = initEverything
    result = addToDo description position everything
  result === Everything { inbox = [], queue  = [ToDo {_description = "TEST"}], notes  = [], habits = [], async = [], thrash = []}




main :: IO Bool
main =
  checkParallel $ Group "Test.Example" [
      -- ("init_everything", init_everything),
      ("testAddToDo", testAddToDo)
    ]
