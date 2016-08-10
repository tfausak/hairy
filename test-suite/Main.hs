{-# LANGUAGE OverloadedStrings #-}

module Main
  ( main
  ) where

import Control.Monad.Reader (runReaderT)
import Hairy (Config (Config), Environment (Test), application, pool,
  environment, getPool, runConfigM)
import Hairy.Models (Task (Task), taskContent, taskCreated)
import Data.Time.Calendar (fromGregorian)
import Data.Time.Clock (UTCTime (UTCTime), utctDay, utctDayTime)
import Database.Persist.Sql (ConnectionPool, insert_, rawExecute, runSqlPool)
import Network.HTTP.Types.Status (created201, notFound404, ok200)
import Network.Wai.Internal (requestMethod)
import Network.Wai.Test (SRequest (SRequest), defaultRequest, request,
  runSession, setPath, simpleBody, simpleHeaders, simpleRequest,
  simpleRequestBody, simpleStatus, srequest)
import Test.Hspec (before, describe, hspec, it, shouldBe)
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
  a <- scottyAppT t (application c)

  hspec $ before (resetDB p) $ do
    describe "/" $ do
      describe "GET" $ do
        it "404s" $ do
          r <- runSession (request defaultRequest) a
          simpleStatus r `shouldBe` notFound404
          lookup "Content-Type" (simpleHeaders r) `shouldBe`
            Just "application/json; charset=utf-8"
          simpleBody r `shouldBe` "null"

    describe "/tasks" $ do
      describe "GET" $ do
        it "lists the tasks" $ do
          r <- runSession (request (defaultRequest `setPath` "/tasks")) a
          simpleStatus r `shouldBe` ok200
          lookup "Content-Type" (simpleHeaders r) `shouldBe`
            Just "application/json; charset=utf-8"
          simpleBody r `shouldBe` "[]"

      describe "POST" $ do
        it "creates a task" $ do
          let req = srequest SRequest
                { simpleRequest = defaultRequest
                  { requestMethod = "POST"
                  } `setPath` "/tasks"
                , simpleRequestBody = "{\"content\":\"\",\"created\":\"2001-02-03T04:05:06Z\"}"
                }
          r <- runSession req a
          simpleStatus r `shouldBe` created201
          lookup "Content-Type" (simpleHeaders r) `shouldBe`
            Just "application/json; charset=utf-8"
          simpleBody r `shouldBe`
            "{\"created\":\"2001-02-03T04:05:06.000Z\",\"content\":\"\"}"

    describe "/tasks/:id" $ before (createTask p) $ do
      describe "GET" $ do
        it "shows the task" $ do
          r <- runSession (request (setPath defaultRequest "/tasks/1")) a
          simpleStatus r `shouldBe` ok200
          lookup "Content-Type" (simpleHeaders r) `shouldBe`
            Just "application/json; charset=utf-8"
          simpleBody r `shouldBe`
            "{\"created\":\"2001-02-03T04:05:06.000Z\",\"content\":\"\"}"

      describe "PUT" $ do
        it "replaces the task" $ do
          let req = srequest SRequest
                { simpleRequest = defaultRequest
                  { requestMethod = "PUT"
                  } `setPath` "/tasks/1"
                , simpleRequestBody = "{\"content\":\"!\",\"created\":\"2001-02-03T04:05:06Z\"}"
                }
          r <- runSession req a
          simpleStatus r `shouldBe` ok200
          lookup "Content-Type" (simpleHeaders r) `shouldBe`
            Just "application/json; charset=utf-8"
          simpleBody r `shouldBe`
            "{\"created\":\"2001-02-03T04:05:06.000Z\",\"content\":\"!\"}"

      describe "DELETE" $ do
        it "deletes the task" $ do
          r <- runSession (request (defaultRequest { requestMethod = "DELETE" }`setPath` "/tasks/1")) a
          simpleStatus r `shouldBe` ok200
          lookup "Content-Type" (simpleHeaders r) `shouldBe`
            Just "application/json; charset=utf-8"
          simpleBody r `shouldBe` "null"

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
