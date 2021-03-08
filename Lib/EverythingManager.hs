module Lib.EverythingManager where

import Control.Lens
import Data.Generics.Product
import Data.Text
import Data.Tree
import GHC.Generics
import Numeric.Natural
import Opaleye as O
import Data.Profunctor.Product (p2)
import Database.PostgreSQL.Simple (Connection, connect, ConnectInfo(..))
import Lib.Connection

-- This application is strongly connected to the following
-- * human level AI
-- * the way human beings optimally make decisions
-- * and operating system architecture
-- Consider them when thinking about what needs to be modeled

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



toDoTable :: Table (Field SqlInt4, Field SqlText) (Field SqlInt4, Field SqlText)
toDoTable = Table "toDo" (p2 (
    tableField "id"
    ,tableField "description"))

selectAllToDos :: Connection -> IO [(Int, Text)]
selectAllToDos conn =
  O.runSelect conn $
  O.selectTable toDoTable

main = do
  c <- getDbConn
  [(toDoId, toDoDescription),_] <- selectAllToDos c
  putStrLn $ show toDoDescription

-- TODO both ToDos and Notes should be trees
-- |Core type of actionable ToDo's and non-actionable information storage
data Item = Item {
  toDo :: ToDo,
  note :: Note
} deriving (Show)

-- |Information that you want to act upon
newtype ToDo = ToDo {
  _description :: Text
} deriving (Show, Generic)

-- |Information that you store without initial intention to act upon it
newtype Note = Note {
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



-- |Add a `ToDo` in the chosen place in the `Queue`
addToDo :: ToDo -> Maybe Priority -> Everything -> Everything
addToDo todo priority everything = undefined

-- |Grooming action run before attempting to execute
-- the `ToDo` with `Priority` of 0
startToDo :: Everything -> IO Everything
startToDo everything = undefined
-- Decent implementation:
-- Move external InboxItems to Inbox
-- Prioritize some items in Inbox
-- Iterate over top n in Queue
-- Let n = 10 cause we are humans
