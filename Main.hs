module Main where

import Data.Text

-- priority queue
-- inbox
-- information storage
-- adding a new task
-- starting work on a task

-- analogous to Code | Data
data Item = ToDo | Data

newtype ToDo = ToDoNew {
  toDoDescription :: Text
} deriving (Show)

newtype Data = DataNew {
  dataDescription :: Text
} deriving (Show)

newtype InboxItem = InboxItemNew {
  inboxItemDescription :: Text
} deriving (Show)

-- TODO replace with priority list with good performance characteristics
-- by convention highest priority is head
type PriorityQueue = [ToDo]

type Inbox = [InboxItem]



main :: IO ()
main = putStrLn "Hello, Haskell!"
