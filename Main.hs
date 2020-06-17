module Main where

-- priority queue
-- inbox
-- information storage
-- adding a new task
-- starting work on a task

-- analogous to Code | Data
data Item = ToDo | Data

data ToDo = ToDo {
  description :: Text
} deriving (Show)

-- TODO replace with priority list with good performance characteristics
type PriorityQueue = [ToDo]





main :: IO ()
main = putStrLn "Hello, Haskell!"
