module Main where

import Data.Text
import Numeric.Natural

-- This application is strongly connected to the following
-- * human level AI
-- * the way human beings optimally make decisions
-- * and operating system architecture
-- Consider them when thinking about what needs to be modeled

-- TODO Model it, initially using a tree, later maybe a graph, ...
-- |Every `ToDo` and `Note` stored in the app
data Everything = Everything

-- |Core type of actionable ToDo's and non-actionable information storage
data Item = Item {
  toDo :: ToDo,
  note :: Note
}

-- QUESTION I think I run into the records problem.
-- How do I use `description` for all of the bellow
-- instead of having to name them differently

-- TODO equivalent to inheritance
-- `ToDo` and `Note` should inherit from `TextInformation`
-- so that the `text` field should not have to be
-- defined 2 times which is code duplication
-- could also try: https://gitlab.haskell.org/ghc/ghc/-/wikis/records/overloaded-record-fields/duplicate-record-fields
-- not sure if good idea

-- |Information that you want to act upon
newtype ToDo = ToDo {
  toDoText :: Text
} deriving (Show)

-- |Information that you store for later reference
newtype Note = Note {
  noteText :: Text
} deriving (Show)

-- |Item which was not immediately prioritized
type InboxItem = Item

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
addToDo todo priority queue = undefined

-- |Grooming action run before attempting to execute
-- the `ToDo` with `Priority` of 0
startToDo :: Everything -> IO Everything
startToDo everything = undefined









main :: IO ()
main = putStrLn "Hello, Haskell!"