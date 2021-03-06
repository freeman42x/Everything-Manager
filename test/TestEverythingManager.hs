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
  let
    description = "TEST"
    position = 0
    everything = initEverything
    result = addToDo description position everything
  result === Everything { inbox = [], queue  = [ToDo {_description = "TEST"}], notes  = [], habits = [], async = [], thrash = []}

testMoveToDo :: Property
testMoveToDo =
  property $ do
  let
    position1 = 0
    position2 = 1
    everything = Everything {inbox = [], queue = [ToDo {_description = "TEST"},ToDo {_description = "TEST2"}], notes = [], habits = [], async = [], thrash = []}
    result = moveToDo position1 position2 everything
  result === Everything {inbox = [], queue = [ToDo {_description = "TEST2"},ToDo {_description = "TEST"}], notes = [], habits = [], async = [], thrash = []}


testEditToDo :: Property
testEditToDo =
  property $ do
  let
    position = 0
    description = "TESTEDIT"
    everything = Everything { inbox = [], queue  = [ToDo {_description = "TEST"}], notes  = [], habits = [], async = [], thrash = []}
    result = editToDo description position everything
  result === Everything { inbox = [], queue  = [ToDo {_description = "TESTEDIT"}], notes  = [], habits = [], async = [], thrash = []}




testAddInbox :: Property
testAddInbox =
  property $ do
  let
    toDoDescription = "TEST"
    noteDescription = "TEST2"
    position = 0
    everything = initEverything
    result = addInbox toDoDescription noteDescription position everything
  result === Everything {inbox = [Item {toDo = ToDo {_description = "TEST"}, note = Note {_description = "TEST2"}}], queue = [], notes = [], habits = [], async = [], thrash = []}


tests :: IO Bool
tests =
  checkParallel $ Group "Test.Example" [
      -- ("init_everything", init_everything),
      ("testAddToDo", testAddToDo),
      ("testMoveToDo", testMoveToDo),
      ("testEditToDo", testEditToDo),
      ("testAddInbox", testAddInbox)
    ]
