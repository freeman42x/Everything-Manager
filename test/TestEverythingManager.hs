import           Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Lib.EverythingManager
import Control.Monad.IO.Class
import Opaleye.Manipulation
import Database.PostgreSQL.Simple (Connection, connect, ConnectInfo(..))
import Lib.Connection
import Control.Monad (void)
import Opaleye as O


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

testMoveTodo :: Property
testMoveToDo =
  property $ do
  let
    position1 = 0
    position2 = 1
    everything = Everything {inbox = [], queue = [ToDo {_description = "TEST"},ToDo {_description = "TEST2"}], notes = [], habits = [], async = [], thrash = []}
    result = addToDo description position everything
  result === Everything {inbox = [], queue = [ToDo {_description = "TEST2"},ToDo {_description = "TEST"}], notes = [], habits = [], async = [], thrash = []}


testEditTodo :: Property
testEditTodo
  property $ do
  let
    position = 0
    description = "TESTEDIT"
    everything = Everything { inbox = [], queue  = [ToDo {_description = "TEST"}], notes  = [], habits = [], async = [], thrash = []}
    result = editToDo description position everything
  result === Everything { inbox = [], queue  = [ToDo {_description = "TESTEDIT"}], notes  = [], habits = [], async = [], thrash = []}

runClearDb :: Connection -> IO ()
runClearDb conn = do
  clearToDoTable conn
  clearNotesTable conn

-- -- -- Start with empty everything. add a todo. ensure todo is successfully added to everything
-- testAddToDo :: Property
-- testAddToDo =
--   property $ do
--   let
--     description = "TEST"
--     position = 0
--     everything = loadEverything
--     result = addToDo description position everything
--   result === Everything { inbox = [], queue  = [ToDo {_description = "TEST"}], notes  = [], habits = [], async = [], thrash = []}

<<<<<<< HEAD
-- testMoveToDo :: Property
-- testMoveToDo =
--   property $ do
--   let
--     position1 = 0
--     position2 = 1
--     everything = Everything {inbox = [], queue = [ToDo {_description = "TEST"},ToDo {_description = "TEST2"}], notes = [], habits = [], async = [], thrash = []}
--     result = moveToDo position1 position2 everything
--   result === Everything {inbox = [], queue = [ToDo {_description = "TEST2"},ToDo {_description = "TEST"}], notes = [], habits = [], async = [], thrash = []}


-- testEditToDo :: Property
-- testEditToDo =
--   property $ do
--   let
--     position = 0
--     description = "TESTEDIT"
--     everything = Everything { inbox = [], queue  = [ToDo {_description = "TEST"}], notes  = [], habits = [], async = [], thrash = []}
--     result = editToDo description position everything
--   result === Everything { inbox = [], queue  = [ToDo {_description = "TESTEDIT"}], notes  = [], habits = [], async = [], thrash = []}




-- testAddInbox :: Property
-- testAddInbox =
--   property $ do
--   let
--     toDoDescription = "TEST"
--     noteDescription = "TEST2"
--     position = 0
--     everything = initEverything
--     result = addInbox toDoDescription noteDescription position everything
--   result === Everything {inbox = [Item {toDo = ToDo {_description = "TEST"}, note = Note {_description = "TEST2"}}], queue = [], notes = [], habits = [], async = [], thrash = []}


-- Call runClearDb after every unit test that uses the database.
-- TODO make an abstraction that does this better
main :: IO Bool
main =
=======

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
>>>>>>> 15b0600... 13 - Add unit tests
>>>>>>> e5d7260... 13 - Add unit tests
  checkParallel $ Group "Test.Example" [
      -- ("init_everything", init_everything),
      ("testAddToDo", testAddToDo),
      ("testMoveToDo", testMoveToDo),
      ("testEditToDo", testEditToDo),
      ("testAddInbox", testAddInbox)
    ]
main = do
  c <- testDbConn
  runClearDb c
  check $ testInsertAndSelectInboxById c
