> {-# LANGUAGE FlexibleContexts #-}
> {-# LANGUAGE GeneralizedNewtypeDeriving #-}
> {-# LANGUAGE OverloadedStrings #-}

> module Hairy where

> import Control.Applicative (Applicative)
> import Control.Monad.IO.Class (MonadIO, liftIO)
> import Control.Monad.Logger (runNoLoggingT, runStdoutLoggingT)
> import Control.Monad.Reader (MonadReader, ReaderT, asks, runReaderT)
> import Control.Monad.Trans.Class (MonadTrans, lift)
> import Data.Aeson (Value (Null), (.=), object)
> import Data.Default (def)
> import Data.Text.Lazy (Text)
> import qualified Database.Persist as DB
> import qualified Database.Persist.Postgresql as DB
> import Hairy.Models (Task, TaskId, migrateAll)
> import Network.HTTP.Types.Status (created201, internalServerError500,
>   notFound404)
> import Network.Wai (Middleware)
> import Network.Wai.Handler.Warp (defaultSettings)
> import Network.Wai.Middleware.RequestLogger (logStdout, logStdoutDev)
> import System.Environment (lookupEnv)
> import Web.Scotty.Trans (ActionT, Options, ScottyT, defaultHandler, delete,
>   get, json, jsonData, middleware, notFound, param, post, put, scottyOptsT,
>   settings, showError, status, verbose)

> main :: IO ()
> main = do
>   c <- getConfig
>   runApplication c

> getConfig :: IO Config
> getConfig = do
>   e <- getEnvironment
>   p <- getPool e
>   return Config
>     { environment = e
>     , pool = p
>     }

> data Config = Config
>   { environment :: Environment
>   , pool :: DB.ConnectionPool
>   }

> getEnvironment :: IO Environment
> getEnvironment = do
>   m <- lookupEnv "SCOTTY_ENV"
>   let e = case m of
>         Nothing -> Development
>         Just s -> read s
>   return e

> getPool :: Environment -> IO DB.ConnectionPool
> getPool e =
>   case e of
>     Development -> runStdoutLoggingT (DB.createPostgresqlPool s n)
>     Production -> runStdoutLoggingT (DB.createPostgresqlPool s n)
>     Test -> runNoLoggingT (DB.createPostgresqlPool s n)
>   where
>     s = getConnectionString e
>     n = getConnectionSize e

> getConnectionString :: Environment -> DB.ConnectionString
> getConnectionString Development =
>   "host=localhost port=5432 user=postgres dbname=hairy_development"
> getConnectionString Production =
>   "host=localhost port=5432 user=postgres dbname=hairy_production"
> getConnectionString Test =
>   "host=localhost port=5432 user=postgres dbname=hairy_test"

> getConnectionSize :: Environment -> Int
> getConnectionSize Development = 1
> getConnectionSize Production = 8
> getConnectionSize Test = 1

> data Environment
>   = Development
>   | Production
>   | Test
>   deriving (Eq, Read, Show)

> runApplication :: Config -> IO ()
> runApplication c = do
>   let o = getOptions (environment c)
>       r m = runReaderT (runConfigM m) c
>   scottyOptsT o r r application

> -- GeneralizedNewtypeDeriving
> newtype ConfigM a = ConfigM
>  { runConfigM :: ReaderT Config IO a
>  } deriving (Applicative, Functor, Monad, MonadIO, MonadReader Config)

> getOptions :: Environment -> Options
> getOptions Development = def
> getOptions Production = def
>   { settings = defaultSettings
>   , verbose = 0
>   }
> getOptions Test = def
>   { verbose = 0
>   }

> type Error = Text

> application :: ScottyT Error ConfigM ()
> application = do
>   runDB (DB.runMigration migrateAll)

>   e <- lift (asks environment)
>   middleware (loggingM e)
>   defaultHandler (defaultH e)

>   get "/tasks" getTasksA
>   post "/tasks" postTasksA
>   get "/tasks/:id" getTaskA
>   put "/tasks/:id" putTaskA
>   delete "/tasks/:id" deleteTaskA

>   notFound notFoundA

> runDB :: (MonadTrans t, MonadIO (t ConfigM)) => DB.SqlPersistT IO a -> t ConfigM a
> runDB q = do
>   p <- lift (asks pool)
>   liftIO (DB.runSqlPool q p)

> loggingM :: Environment -> Middleware
> loggingM Development = logStdoutDev
> loggingM Production = logStdout
> loggingM Test = id

> type Action = ActionT Error ConfigM ()

> defaultH :: Environment -> Error -> Action
> defaultH e x = do
>   status internalServerError500
>   case e of
>     Development -> json (object ["error" .= showError x])
>     Production -> json Null
>     Test -> json (object ["error" .= showError x])

> getTasksA :: Action
> getTasksA = do
>   ts <- runDB (DB.selectList [] [])
>   json (ts :: [DB.Entity Task])

> postTasksA :: Action
> postTasksA = do
>   t <- jsonData
>   runDB (DB.insert_ t)
>   status created201
>   json (t :: Task)

> getTaskA :: Action
> getTaskA = do
>   i <- param "id"
>   m <- runDB (DB.get (toKey i))
>   case m of
>     Nothing -> notFoundA
>     Just t -> json (t :: Task)

> putTaskA :: Action
> putTaskA = do
>   i <- param "id"
>   t <- jsonData
>   runDB (DB.repsert (toKey i) t)
>   json (t :: Task)

> deleteTaskA :: Action
> deleteTaskA = do
>   i <- param "id"
>   runDB (DB.delete (toKey i :: TaskId))
>   json Null

> -- FlexibleContexts
> toKey :: DB.ToBackendKey DB.SqlBackend a => Integer -> DB.Key a
> toKey i = DB.toSqlKey (fromIntegral (i :: Integer))

> notFoundA :: Action
> notFoundA = do
>   status notFound404
>   json Null
