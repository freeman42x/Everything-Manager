module Main where

import Data.Text

-- priority queue
-- inbox
-- information storage
-- adding a new task
-- starting work on a task

-- |Core type of ToDo's and non-ToDo's information storage
data Item = ToDo | Data

-- |Information that you want to act upon
newtype ToDo = ToDoNew {
  toDoDescription :: Text
} deriving (Show)

-- |Information that you do not want to act upon
newtype Data = DataNew {
  dataDescription :: Text
} deriving (Show)

-- |Item which was not immediately prioritized
newtype InboxItem = InboxItemNew {
  inboxItemDescription :: Text
} deriving (Show)

-- TODO replace with priority list with good performance characteristics
-- by convention highest priority is head
-- |This is where you will be prioritizing and executing the
-- most important tasks from
type PriorityQueue = [ToDo]

-- |This is where you will add an `InboxItem` when you
-- do not immediately prioritize an `Item`
type Inbox = [InboxItem]



main :: IO ()
main = putStrLn "Hello, Haskell!"
