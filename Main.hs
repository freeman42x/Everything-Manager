module Main where

import Data.Text
import Numeric.Natural

-- queue
-- inbox
-- information storage
-- adding a new task
-- starting work on a task

-- TODO Model it, initially using a tree, later maybe a graph, ...
-- |Every `ToDo` and `Data` stored in the app
data Everything = Everything

-- |Core type of ToDo's and non-ToDo's information storage
data Item = Item {
  toDo :: ToDo,
  dataNew :: Data
}

-- QUESTION I think I run into the records problem.
-- How do I use `description` for all of the bellow
-- instead of having to name them differently

-- TODO equivalent to inheritance
-- all 3 bellow should inherit from `TextInformation`
-- so that the `text` field should not have to be
-- defined 3 times
-- could also try: https://gitlab.haskell.org/ghc/ghc/-/wikis/records/overloaded-record-fields/duplicate-record-fields
-- not sure if good idea

-- TODO model structure, probably good start would be a tree
-- |Information that you want to act upon
newtype ToDo = ToDo {
  toDoText :: Text
} deriving (Show)

-- |Information that you do not want to act upon
newtype Data = Data {
  dataText :: Text
} deriving (Show)

-- |Item which was not immediately prioritized
type InboxItem = ToDo

-- TODO replace with queue with good performance characteristics
-- by convention highest priority is head
-- Data.Sequence, Data.Queue or alternatives
-- |This is where you will be prioritizing and executing the
-- most important tasks from
type Queue = [ToDo]

-- |This is where you will add an `InboxItem` when you
-- do not immediately prioritize an `Item`
type Inbox = [InboxItem]

-- |Priority of 0 is highest Priority
-- Optimal is to always execute priority 0 next
type Priority = Natural

-- TODO ensure this is a total function and will not throw errors
-- |Add a `ToDo` in the chosen place in the `Queue`
addToDo :: ToDo -> Priority -> Queue -> Queue
addToDo = undefined

-- |Grooming action run before attempting to execute
-- the `ToDo` with `Priority` of 0
startToDo :: Everything -> IO Everything
startToDo = undefined









main :: IO ()
main = putStrLn "Hello, Haskell!"