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


testInsertAndSelectInboxById :: Connection -> Property
testInsertAndSelectInboxById conn = property $ do
    toDoDescription <- forAll $ Gen.text (Range.linear 0 100) Gen.alpha
    noteDescription <- forAll $ Gen.text (Range.linear 0 100) Gen.alpha
    note_id <- liftIO $ insertInbox conn toDoDescription noteDescription
    i <- liftIO $ selectInboxById conn note_id
    let (Item (ToDo id1 desc1) (Note id2 desc2)) = i
    i === Item { toDo = ToDo {id = id1, _description = toDoDescription}, note = Note {id = id2, _description = noteDescription}}

clearToDoTable :: Connection -> IO ()
clearToDoTable conn = do
  void $ runDelete_ conn del
  where del = Delete {
          dTable = toDoTable
          -- use statement that always evals to true, could be improved
          ,dWhere = (\(iDb, _, _, _) -> iDb .== iDb)
          , dReturning = rCount
  }
clearNotesTable :: Connection -> IO ()
clearNotesTable conn = do
  void $ runDelete_ conn del
  where del = Delete {
          dTable = notesTable
          -- use statement that always evals to true, could be improved
          ,dWhere = (\(iDb, _, _, _) -> iDb .== iDb)
          , dReturning = rCount
  }

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
  checkParallel $ Group "Test.Example" [
      ("init_everything", init_everything)
      ("add_todo", add_todo)
    ]
main = do
  c <- testDbConn
  runClearDb c
  check $ testInsertAndSelectInboxById c
