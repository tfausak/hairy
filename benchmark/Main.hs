{-# LANGUAGE OverloadedStrings #-}

module Main
  ( main
  ) where

import Control.Monad.Reader (runReaderT)
import Criterion (bench, bgroup, whnfIO)
import Criterion.Main (defaultMain)
import Data.Time.Calendar (fromGregorian)
import Data.Time.Clock (UTCTime (UTCTime), utctDay, utctDayTime)
import Database.Persist.Sql (ConnectionPool, insert_, rawExecute, runSqlPool)
import Hairy (Config (Config), Environment (Test), application, pool,
  environment, getPool, runConfigM)
import Hairy.Models (Task (Task), taskContent, taskCreated)
import Network.Wai.Internal (requestMethod)
import Network.Wai.Test (SRequest (SRequest), defaultRequest, request,
  runSession, setPath, simpleRequest, simpleRequestBody, srequest)
import Web.Scotty.Trans (scottyAppT)

main :: IO ()
main = do
  let e = Test
  p <- getPool e
  let c = Config
        { environment = e
        , pool = p
        }
      t m = runReaderT (runConfigM m) c
  a <- scottyAppT t t application

  resetDB p
  createTask p

  defaultMain
    [ bgroup "/"
      [ bench "GET" $ whnfIO $ do
        runSession (request defaultRequest) a
      ]
    , bgroup "/tasks"
      [ bench "GET" $ whnfIO $ do
        runSession (request (defaultRequest `setPath` "/tasks")) a
      , bench "POST" $ whnfIO $ do
        flip runSession a $ srequest SRequest
          { simpleRequest = defaultRequest
            { requestMethod = "POST"
            } `setPath` "/tasks"
          , simpleRequestBody = "{\"content\":\"\",\"created\":\"2001-02-03T04:05:06Z\"}"
          }
      ]
    , bgroup "/tasks/:id"
      [ bench "GET" $ whnfIO $ do
        runSession (request (setPath defaultRequest "/tasks/1")) a
      , bench "PUT" $ whnfIO $ do
        flip runSession a $ srequest SRequest
          { simpleRequest = defaultRequest
            { requestMethod = "PUT"
            } `setPath` "/tasks/1"
          , simpleRequestBody = "{\"content\":\"\",\"created\":\"2001-02-03T04:05:06Z\"}"
          }
      , bench "DELETE" $ whnfIO $ do
        flip runSession a $ request $ defaultRequest
          { requestMethod = "DELETE"
          } `setPath` "/tasks/1"
      ]
    ]

resetDB :: ConnectionPool -> IO ()
resetDB = runSqlPool (rawExecute "TRUNCATE TABLE task RESTART IDENTITY" [])

createTask :: ConnectionPool -> IO ()
createTask = runSqlPool (insert_ task) where
  task = Task
    { taskContent = ""
    , taskCreated = UTCTime
      { utctDay = fromGregorian 2001 2 3
      , utctDayTime = 14706
      }
    }
