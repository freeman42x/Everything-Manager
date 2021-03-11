module Lib.EverythingManager where

import Control.Lens
import Data.Generics.Product
import Data.Text hiding (take, drop, map)
import Data.Tree
import GHC.Generics
import Numeric.Natural
import Opaleye as O
import Data.Profunctor.Product (p4)
import Database.PostgreSQL.Simple (Connection, connect, ConnectInfo(..))
import Control.Arrow (returnA)
import Control.Monad (void)
import Opaleye.FunctionalJoin (joinF)
import Lib.Connection
import Data.Int
-- This application is strongly connected to the following
-- * human level AI
-- * the way human beings optimally make decisions
-- * and operating system architecture
-- Consider them when thinking about what needs to be modeled


inboxIndex = 0
queueIndex = 1
noteIndex = 2
habitIndex = 3
asyncIndex = 4
thrashIndex = 5


-- TODO Model it, initially using a tree, later maybe a graph, ...
-- |Every `ToDo` and `Note` stored in the app
data Everything = Everything {
  inbox  :: Inbox,
  queue  :: Queue,
  notes  :: Forest Note,
  habits :: Habits,
  async  :: Async,
  thrash :: Forest Thrash
} deriving (Show)



-- TODO both ToDos and Notes should be trees
-- |Core type of actionable ToDo's and non-actionable information storage
data Item = Item {
  toDo :: ToDo,
  note :: Note
} deriving (Show)

-- |Information that you want to act upon
data ToDo = ToDo {
  id :: Int,
  _description :: Text
} deriving (Show, Generic)

-- |Information that you store without initial intention to act upon it
data Note = Note {
  id :: Int,
  _description :: Text
} deriving (Show, Generic)

description :: HasField "_description" s t a b => Lens s t a b
description = field @"_description"

-- |ToDos run by external agents or systems
-- are being queued here
-- they might need to be pinged and have callbacks
type Async = [ToDo]

-- TODO model the periodicity
-- |ToDos that need to be done periodically
type Habits = [ToDo]

-- |Items are moved here when they are deemed
-- to never be useful in the future
type Thrash = [Item]

-- TODO replace with queue with good performance characteristics
-- by convention highest priority is head
-- Data.Sequence, Data.Queue or alternatives
-- |This is where you will be prioritizing and executing the
-- most important tasks from
type Queue = [ToDo]

-- TODO should be of small fixed size to not overburden the user mentally
-- |This is where you will add an `InboxItem` when you
-- do not immediately prioritize an `Item`
type Inbox = [Item]

-- |Priority of 0 is highest Priority
-- Optimal is to always execute priority 0 next
type Priority = Natural


toDoTable :: Table (Maybe (Field SqlInt4), Field SqlText, Field SqlInt4, Field SqlInt4) (Field SqlInt4, Field SqlText, Field SqlInt4, Field SqlInt4)
toDoTable = Table "toDo" (p4 (
    tableField "id"
    ,tableField "description"
    ,tableField "type"
    ,tableField "deleted"))

notesTable :: Table (Maybe (Field SqlInt4), Field SqlText, Field SqlInt4, Field SqlInt4) (Field SqlInt4, Field SqlText, Field SqlInt4, Field SqlInt4)
notesTable = Table "notes" (p4 (
    tableField "id"
    ,tableField "description"
    ,tableField "todo_id"
    ,tableField "deleted"))


selectToDoByType :: Connection -> Int -> IO [(Int, Text)]
selectToDoByType conn toDoType' = O.runSelect conn $ proc () -> do
  row@(id,description,toDoType, deleted) <- O.selectTable toDoTable -< ()
  restrict -< (toDoType .== O.toFields toDoType')
  restrict -< (deleted .== O.toFields (0 :: Int))
  returnA -< (id, description)

selectAllNotes :: Connection -> IO [(Int, Text, Int, Int)]
selectAllNotes conn =
  O.runSelect conn $
  O.selectTable notesTable

selectAllToDos :: Connection -> IO [(Int, Text, Int, Int)]
selectAllToDos conn =
  O.runSelect conn $
  O.selectTable toDoTable

selectNoteByType :: Connection -> Int -> IO [(Int, Text, Int, Text)]
selectNoteByType conn toDoType = O.runSelect conn $ proc () -> do
  row1@(id1, noteDescription,toDoId, deleted1) <- O.selectTable notesTable -< ()
  row2@(id2,toDoDescription,toDoType', deleted2) <- O.selectTable toDoTable -< ()
  restrict -< (toDoType' .== O.toFields toDoType)
  restrict -< (deleted1 .== O.toFields (0 :: Int))
  restrict -< (deleted2 .== O.toFields (0 :: Int))
  restrict -< (toDoId .== id2)
  returnA -< (id1, noteDescription, toDoId, toDoDescription)

selectNoteById :: Connection -> Int -> IO [(Int, Text, Int, Text)]
selectNoteById conn id = O.runSelect conn $ proc () -> do
  row1@(id1, noteDescription, toDoId, deleted1) <- O.selectTable notesTable -< ()
  row2@(id2, toDoDescription, toDoType', deleted2) <- O.selectTable toDoTable -< ()
  restrict -< (id1 .== O.toFields id)
  restrict -< (toDoId .== id2)
  returnA -< (id1, noteDescription, toDoId, toDoDescription)

insertToDo :: Connection -> Text -> Int -> IO [(Int)]
insertToDo conn description toDoType = do
  runInsert_ conn ins
  where
    ins = Insert {
          iTable = toDoTable
          ,iRows = [(Nothing, toFields description, toFields toDoType, toFields (0 :: Int))]
          ,iReturning = rReturning (\(id, _, _, _) -> id)
          ,iOnConflict = Nothing
    }

updateToDo :: Connection -> Int -> Text -> Int -> IO ()
updateToDo conn id description toDoType =
  void $ runUpdate_ conn u
  where
    u = Update {
      uTable = toDoTable
      ,uUpdateWith = (\(id_,_,_,deleted) -> (Just id_, toFields description, toFields toDoType, deleted))
      ,uWhere = (\(id_,_,_,_) -> id_ .== toFields id)
      ,uReturning = rCount
    }

deleteToDo :: Connection -> Int -> IO ()
deleteToDo conn id =
  void $ runUpdate_ conn u
  where
    u = Update {
        uTable = toDoTable
        ,uUpdateWith = (\(id_,description,toDoType,deleted) -> (Just id_, description, toDoType, toFields (1 :: Int)))
        ,uWhere = (\(id_,_,_,_) -> id_ .== toFields id)
        ,uReturning = rCount
    }


insertNote :: Connection -> Text -> Int -> IO ()
insertNote conn noteDescription toDoId =
  void $ runInsert_ conn ins
  where
    ins = Insert {
      iTable = notesTable
      ,iRows = [(Nothing, toFields noteDescription, toFields toDoId, toFields (0 :: Int))]
      ,iReturning = rCount
      ,iOnConflict = Nothing
    }

updateNote :: Connection -> Int -> Text -> Int -> IO ()
updateNote conn id description toDoId =
  void $ runUpdate_ conn u
  where
    u = Update {
      uTable = notesTable
      ,uUpdateWith = (\(id_,_,_,deleted) -> (Just id_, toFields description, toFields toDoId, deleted))
      ,uWhere = (\(id_,_,_,_) -> id_ .== toFields id)
      ,uReturning = rCount
      }

deleteNote :: Connection -> Int -> IO ()
deleteNote conn id =
  void $ runUpdate_ conn u
  where
    u = Update {
        uTable = notesTable
        ,uUpdateWith = (\(id_,description,toDoId,deleted) -> (Just id_, description, toDoId, toFields (1 :: Int)))
        ,uWhere = (\(id_,_,_,_) -> id_ .== toFields id)
        ,uReturning = rCount
    }


selectInbox :: Connection -> IO [Item]
selectInbox conn = do
  items <- selectNoteByType conn inboxIndex
  return (map (\(noteId, noteDescription, toDoId, toDoDescription) ->
         Item { toDo = ToDo {id = toDoId, _description = toDoDescription},
                note = Note {id = noteId, _description = noteDescription}
              }) items)

insertInbox :: Connection -> Text -> Text -> IO ()
insertInbox conn toDoDescription noteDescription = do
  [(toDoId)] <- insertToDo conn toDoDescription inboxIndex
  insertNote conn noteDescription toDoId

updateInbox :: Connection -> Int -> Text -> Int -> Int -> Text -> IO ()
updateInbox conn toDoId toDoDescription toDoType noteId noteDescription = do
  updateToDo conn toDoId toDoDescription toDoType
  updateNote conn noteId noteDescription toDoId

deleteInbox :: Connection -> Int -> Int -> IO ()
deleteInbox conn toDoId noteId = do
  deleteToDo conn toDoId
  deleteNote conn noteId

selectQueue :: Connection -> IO [ToDo]
selectQueue conn = do
  items <- selectToDoByType conn queueIndex
  return (map (\(toDoId, toDoDescription) ->
          ToDo {id = toDoId, _description = toDoDescription})
         items)

insertQueue :: Connection -> Text -> IO [(Int)]
insertQueue conn description = insertToDo conn description queueIndex

updateQueue :: Connection -> Int -> Text -> Int -> IO ()
updateQueue conn id description toDoType =
  updateToDo conn id description toDoType

deleteQueue :: Connection -> Int -> IO ()
deleteQueue conn toDoId =
  deleteToDo conn toDoId


selectNotes :: Connection -> IO [ToDo]
selectNotes conn = do
  items <- selectToDoByType conn noteIndex
  return (map (\(toDoId, toDoDescription) ->
          ToDo {id = toDoId, _description = toDoDescription})
         items)


insertNotes :: Connection -> Text -> IO [(Int)]
insertNotes conn description = insertToDo conn description noteIndex

updateNotes :: Connection -> Int -> Text -> Int -> IO ()
updateNotes conn id description toDoType =
  updateToDo conn id description toDoType

deleteNotes :: Connection -> Int -> IO ()
deleteNotes conn toDoId =
  deleteToDo conn toDoId


selectHabits :: Connection -> IO [ToDo]
selectHabits conn = do
  items <- selectToDoByType conn habitIndex
  return (map (\(toDoId, toDoDescription) ->
          ToDo {id = toDoId, _description = toDoDescription})
         items)

insertHabits :: Connection -> Text -> IO [(Int)]
insertHabits conn description = insertToDo conn description habitIndex

updateHabits :: Connection -> Int -> Text -> Int -> IO ()
updateHabits conn id description toDoType =
  updateToDo conn id description toDoType

deleteHabits :: Connection -> Int -> IO ()
deleteHabits conn toDoId =
  deleteToDo conn toDoId

selectAsync :: Connection -> IO [ToDo]
selectAsync conn = do
  items <- selectToDoByType conn habitIndex
  return (map (\(toDoId, toDoDescription) ->
          ToDo {id = toDoId, _description = toDoDescription})
         items)


insertAsync :: Connection -> Text -> IO [(Int)]
insertAsync conn description = insertToDo conn description asyncIndex

updateAsync :: Connection -> Int -> Text -> Int -> IO ()
updateAsync conn id description toDoType =
  updateToDo conn id description toDoType

deleteAsync :: Connection -> Int -> IO ()
deleteAsync conn toDoId =
  deleteToDo conn toDoId

selectThrash :: Connection -> IO [Item]
selectThrash conn = do
  items <- selectNoteByType conn thrashIndex
  return (map (\(noteId, noteDescription, toDoId, toDoDescription) ->
         Item { toDo = ToDo {id = toDoId, _description = toDoDescription},
                note = Note {id = noteId, _description = noteDescription}
              }) items)


insertThrash :: Connection -> Text -> Text -> IO ()
insertThrash conn noteDescription toDoDescription = do
  [(toDoId)] <- insertToDo conn toDoDescription thrashIndex
  insertNote conn noteDescription toDoId

updateThrash :: Connection -> Int -> Text -> Int -> Int -> Text -> IO ()
updateThrash conn toDoId toDoDescription toDoType noteId noteDescription = do
  updateToDo conn toDoId toDoDescription toDoType
  updateNote conn noteId noteDescription toDoId

deleteThrash :: Connection -> Int -> Int -> IO ()
deleteThrash conn toDoId noteId = do
  deleteToDo conn toDoId
  deleteNote conn noteId

-- selectEverything :: Everything
-- selectEverything = Everything {
--   inbox = selectInbox,
--   queue  = selectQueue,
--   notes  = selectNotes,
--   habits = selectHabits,
--   async = selectAsync,
--   thrash = selectThrash
-- }


main = do
  c <- getDbConn
  inbox <- selectInbox c
  queue <- selectQueue c
  notes <- selectNotes c
  habits <- selectHabits c
  async <- selectAsync c
  thrash <- selectThrash c
  putStrLn $ "inbox = " ++ show inbox
  putStrLn $ "queue = " ++ show queue
  putStrLn $ "notes = " ++ show notes
  putStrLn $ "habits = " ++ show habits
  putStrLn $ "async = " ++ show async
  putStrLn $ "thrash = " ++ show thrash


-- TODO add error checking for invalid position
addToDo :: Text -> Int -> Everything -> Everything
addToDo description position (Everything inbox queue notes habits async thrash) =
  (Everything inbox ((take position queue) ++
                 [ToDo {_description = description}]
                 ++ (drop position queue)) notes habits async thrash)

addInbox :: Text -> Text -> Int -> Everything -> Everything
addInbox toDoDescription noteDescription position (Everything inbox queue notes habits async thrash) =
  (Everything ((take position inbox) ++
               [Item {toDo = ToDo {_description = toDoDescription},
                      note = Note {_description = noteDescription}}]
                ++ drop position inbox) queue notes habits async thrash)

editToDo = undefined

moveToDo = undefined

-- read everything from database
loadEverything = do
  c <- getDbConn
  inbox <- selectInbox c
  queue <- selectQueue c
  notes  <- selectNotes c
  habits <- selectHabits c
  async <- selectAsync c
  thrash <- selectThrash c
  return Everything {
    inbox = inbox,
    queue = queue,
    notes  = notes,
    habits = habits,
    async = async,
    thrash = thrash
  }

-- |Grooming action run before attempting to execute
-- the `ToDo` with `Priority` of 0
startToDo :: Everything -> IO Everything
startToDo everything = undefined
-- Decent implementation:
-- Move external InboxItems to Inbox
-- Prioritize some items in Inbox
-- Iterate over top n in Queue
-- Let n = 10 cause we are humans
