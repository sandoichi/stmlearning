module MatchNumbers where

import Control.Concurrent.Async (mapConcurrently, concurrently)
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TBMChan ( TBMChan, tryReadTBMChan, readTBMChan
                                       , writeTBMChan, newTBMChan, closeTBMChan )
import Control.Exception (finally)
import Control.Monad (replicateM, forM_)
import Data.List (partition)
import Data.Map (Map, insertWith, empty, toList)
import System.Random (randomRIO)
import Text.Printf (printf)

data MatchResult = Failed | Succeeded Int deriving (Show, Eq)
type ResultMsg = (Int, MatchResult)

numBounds = (0, 1000)

setup :: Int -> Int -> IO (TBMChan Int, TBMChan ResultMsg)
setup workerCount numCount =  do
  resultChan <- atomically $ newTBMChan numCount
  workChan <- atomically $ newTBMChan numCount
  workList <- replicateM numCount $ randomRIO numBounds
  forM_ workList (atomically . writeTBMChan workChan)
  pure (workChan, resultChan)

genMatch :: Int -> Int -> IO MatchResult
genMatch 0 _ = pure Failed
genMatch remainingTries num = do
  result <- randomRIO numBounds
  if result == num
  then pure $ Succeeded num
  else genMatch (remainingTries - 1) num

performWork :: TBMChan Int -> TBMChan ResultMsg -> Int -> IO ()
performWork workChan resultChan workerId = do
  let loop = do
        num <- atomically $ tryReadTBMChan workChan
        case num of
          -- Since our work channel is fully populated from the start,
          -- we only care to read when it is both open and not empty
          Just (Just n) -> do
            result <- genMatch 50 n
            atomically $ writeTBMChan resultChan (workerId, result)
            loop
          _ -> pure ()
  loop

gatherResults :: TBMChan ResultMsg -> IO (Map Int [MatchResult])
gatherResults resultChan = do
  let loop curState = do
        result <- atomically $ readTBMChan resultChan
        case result of
          Nothing -> pure curState
          Just (workerId, res) -> loop $ insertWith (++) workerId [res] curState
  loop empty

formatResults :: (Int, [MatchResult]) -> String
formatResults (workerId, results) =
  let (failures, successes) = partition (== Failed) results
  in printf "\tWorker %i failed %i times, but matched these correctly: %s"
      workerId (length failures) (show $ fmap (\(Succeeded x) -> x) successes)

run :: Int -> Int -> IO ()
run numCount workerCount = do
  (workChan, resultChan) <- setup workerCount numCount

  mapConcurrently (performWork workChan resultChan) [1..workerCount]
    `finally` atomically (closeTBMChan workChan *> closeTBMChan resultChan)

  results <- gatherResults resultChan
  forM_ (toList results)(putStrLn . formatResults)

  pure ()
