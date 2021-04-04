import           Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Lib.EverythingManager
import Control.Monad.IO.Class
import Opaleye.Manipulation
import Database.PostgreSQL.Simple (Connection, connect, ConnectInfo(..))
import Lib.Connection
import Control.Monad (void, forM_)
import Opaleye as O


-- TODO finish implementing
testLoadSettings :: Connection -> Property
testLoadSettings conn = undefined
--   settings <- loadSettings conn
--   settings === Settings { queue_size = 10, theme = dark, hints = 0}

testInsertAndSelectInboxById :: Connection -> Property
testInsertAndSelectInboxById conn = property $ do
    toDoDescription <- forAll $ Gen.text (Range.linear 0 100) Gen.alpha
    noteDescription <- forAll $ Gen.text (Range.linear 0 100) Gen.alpha
    note_id <- liftIO $ insertInbox conn toDoDescription noteDescription
    i <- liftIO $ selectInboxById conn note_id
    let (Item (ToDo id1 desc1) (Note id2 desc2)) = i
    i === Item { toDo = ToDo {id = id1, _description = toDoDescription}, note = Note {id = id2, _description = noteDescription}}

testInsertAndSelectInbox :: Connection -> Property
testInsertAndSelectInbox conn = property $ do
    toDoDescription <- forAll $ Gen.text (Range.linear 0 100) Gen.alpha
    noteDescription <- forAll $ Gen.text (Range.linear 0 100) Gen.alpha
    note_id <- liftIO $ insertInbox conn toDoDescription noteDescription
    is <- liftIO $ selectInbox conn
    length is === 1

testInsertAndSelectQueueById :: Connection -> Property
testInsertAndSelectQueueById conn = property $ do
    toDoDescription <- forAll $ Gen.text (Range.linear 0 100) Gen.alpha
    id <- liftIO $ insertQueue conn toDoDescription
    i <- liftIO $ selectQueueById conn id
    -- let (ToDo id1 desc1) = i
    i === ToDo {id = id, _description = toDoDescription}

testUpdateQueue :: Connection -> Property
testUpdateQueue conn = property $ do
    toDoDescription <- forAll $ Gen.text (Range.linear 0 100) Gen.alpha
    id <- liftIO $ insertQueue conn toDoDescription
    toDoDescription2 <- forAll $ Gen.text (Range.linear 0 100) Gen.alpha
    liftIO $ updateQueue conn id toDoDescription2
    item <- liftIO $ selectQueueById conn id
    item === ToDo {id = id, _description = toDoDescription2}

testMoveQueue :: Connection -> Property
testMoveQueue conn = property $ do
    toDoDescription <- forAll $ Gen.text (Range.linear 0 100) Gen.alpha
    id <- liftIO $ insertQueue conn toDoDescription
    toDoDescription2 <- forAll $ Gen.text (Range.linear 0 100) Gen.alpha
    id2 <- liftIO $ insertQueue conn toDoDescription2
    liftIO $ moveQueue conn id2 0
    items <- liftIO $ selectQueue conn
    items === [ToDo {id = id2, _description = toDoDescription2}, ToDo {id = id, _description = toDoDescription }]

testDeleteQueue :: Connection -> Property
testDeleteQueue conn = property $ do
    toDoDescription <- forAll $ Gen.text (Range.linear 0 100) Gen.alpha
    id <- liftIO $ insertQueue conn toDoDescription
    liftIO $ deleteQueue conn id
    items <- liftIO $ selectQueue conn
    (length items) === 0


-- TODO finish implementing
testAddQueueWhenFull :: Connection -> Property
testAddQueueWhenFull conn = property $ do
    toDoDescription <- forAll $ Gen.text (Range.linear 0 100) Gen.alpha
    id <- liftIO $ forM_ [1..10] $ \_ -> do
      insertQueue conn toDoDescription
    items <- liftIO $ selectQueue conn
    (length items) === 0

testChangeQueueToInbox :: Connection -> Property
testChangeQueueToInbox conn = property $ do
    toDoDescription <- forAll $ Gen.text (Range.linear 0 100) Gen.alpha
    id <- liftIO $ insertQueue conn toDoDescription
    (ToDo id1 desc1) <- liftIO $ selectQueueById conn id
    liftIO $ updateToDo conn id1 desc1 inboxIndex
    items <- liftIO $ selectInbox conn
    (length items) === 1

testInsertAndSelectNotesById :: Connection -> Property
testInsertAndSelectNotesById conn = property $ do
    toDoDescription <- forAll $ Gen.text (Range.linear 0 100) Gen.alpha
    id <- liftIO $ insertNotes conn toDoDescription
    i <- liftIO $ selectNotesById conn id
    let (ToDo id1 desc1) = i
    i === ToDo {id = id1, _description = desc1}

testInsertAndSelectHabitById :: Connection -> Property
testInsertAndSelectHabitById conn = property $ do
    toDoDescription <- forAll $ Gen.text (Range.linear 0 100) Gen.alpha
    id <- liftIO $ insertHabit conn toDoDescription
    i <- liftIO $ selectHabitById conn id
    let (ToDo id1 desc1) = i
    i === ToDo {id = id1, _description = desc1}

testInsertAndSelectAsyncById :: Connection -> Property
testInsertAndSelectAsyncById conn = property $ do
    toDoDescription <- forAll $ Gen.text (Range.linear 0 100) Gen.alpha
    id <- liftIO $ insertAsync conn toDoDescription
    i <- liftIO $ selectAsyncById conn id
    let (ToDo id1 desc1) = i
    i === ToDo {id = id1, _description = desc1}

testInsertAndSelectThrashById :: Connection -> Property
testInsertAndSelectThrashById conn = property $ do
    toDoDescription <- forAll $ Gen.text (Range.linear 0 100) Gen.alpha
    noteDescription <- forAll $ Gen.text (Range.linear 0 100) Gen.alpha
    note_id <- liftIO $ insertThrash conn toDoDescription noteDescription
    i <- liftIO $ selectThrashById conn note_id
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
main = do
  c <- testDbConn
  runClearDb c
  check $ testInsertAndSelectInboxById c
  -- runClearDb c
  -- check $ testInsertAndSelectInbox c
  runClearDb c
  check $ testInsertAndSelectQueueById c
  runClearDb c
  check $ testUpdateQueue c
  runClearDb c
  check $ testMoveQueue c
  runClearDb c
  check $ testDeleteQueue c
  runClearDb c
  check $ testInsertAndSelectNotesById c
  runClearDb c
  check $ testInsertAndSelectHabitById c
  runClearDb c
  check $ testInsertAndSelectAsyncById c
  -- runClearDb c
  -- check $ testInsertAndSelectThrashById c
