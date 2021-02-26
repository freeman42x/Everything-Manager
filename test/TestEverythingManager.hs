{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

import           Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Lib.EverythingManager
import Control.Monad.IO.Class


add_todo :: Property
add_todo =
  property $ do
  description <- forAll $ Gen.list (Range.linear 0 100) Gen.alpha
  priority <- forAll $ Gen.int (Range.linear 0 100)
  -- pure ()
  -- test <- addToDo xs
  startToDo
  liftIO $ putStrLn "asdf"
   

tests :: IO Bool
tests =
  checkParallel $ Group "Test.Example" [
      ("init_everything", init_everything)
      ("add_todo", add_todo)
    ]
