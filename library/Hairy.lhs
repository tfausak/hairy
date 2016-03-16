Before we can begin, we need to enable a few language extensions.

> {-# LANGUAGE OverloadedStrings #-}

This extension allows string literals (like `"cheese"`) to represent string-like
types such as `ByteString` and `Text`. It's not strictly required since you
could do the same thing using `pack`, for instance. But it's so convenient that
it's hard to live without.

> {-# LANGUAGE FlexibleContexts #-}
> {-# LANGUAGE GeneralizedNewtypeDeriving #-}

These are a little harder to explain, so instead I'll explain them when they're
used.

Now we have to let GHC know that our module is called `Hairy`, not `Main` like
it would otherwise assume.

> module Hairy where

Imports make up the last bit of the preamble. These are a little overly-specific
in order to make it easier to see where everything came from. In the real world
you might import everything from, say, `Web.Scotty.Trans` instead of explicitly
listing everything you needed from it.

For the most part, you don't have to worry about these imports. If you're
curious about something later on, come back up here to see where it's imported
from. Then look it up on Hackage.

> import Control.Monad.IO.Class (MonadIO, liftIO)
> import Control.Monad.Logger (runNoLoggingT, runStdoutLoggingT)
> import Control.Monad.Reader (MonadReader, ReaderT, asks, runReaderT)
> import Control.Monad.Trans.Class (MonadTrans, lift)
> import Data.Aeson (Value (Null), (.=), object)
> import Data.Default (def)
> import qualified Data.Text as T
> import Data.Text.Encoding (encodeUtf8)
> import Data.Text.Lazy (Text)
> import qualified Database.Persist as DB
> import qualified Database.Persist.Postgresql as DB
> import Hairy.Models (Task, TaskId, migrateAll)
> import Network.HTTP.Types.Status (created201, internalServerError500,
>   notFound404)
> import Network.Wai (Middleware)
> import Network.Wai.Handler.Warp (Settings, defaultSettings,
>   setFdCacheDuration, setPort)
> import Network.Wai.Middleware.RequestLogger (logStdout, logStdoutDev)
> import System.Environment (lookupEnv)
> import Web.Heroku (parseDatabaseUrl)
> import Web.Scotty.Trans (ActionT, Options, ScottyT, defaultHandler, delete,
>   get, json, jsonData, middleware, notFound, param, post, put, scottyOptsT,
>   settings, showError, status, verbose)

With all that out of the way, we can start on the actual program itself. The
top-level entry point, `main`, only has two responsibilities: get the current
configuration and run the application with that configuration.

> main :: IO ()
> main = do
>   c <- getConfig
>   runApplication c

We could've written this in the point-free style.

    main :: IO ()
    main = getConfig >>= runApplication

Getting the current configuration involves reading the environment from the
system and then setting up the database connection pool. After doing both of
those, we create a new `Config` value with the environment and pool.

> getConfig :: IO Config
> getConfig = do
>   e <- getEnvironment
>   p <- getPool e
>   return Config
>     { environment = e
>     , pool = p
>     }

The data type for `Config` is pretty simple. It has two fields: one for the
environment and one for the database connection pool. We'll define another data
type for the environment, and we're using Persistent's `ConnectionPool` for the
database connection pool.

> data Config = Config
>   { environment :: Environment
>   , pool :: DB.ConnectionPool
>   }

We want to read the environment from the `SCOTTY_ENV` environment variable, then
parse that string as our `Environment` data type and return it. If it doesn't
parse, we'll just blow up.

    $ env SCOTTY_ENV=not-an-environment cabal run
    hairy: Prelude.read: no parse

If we wanted to handle it more gracefully, we could use `Text.Read.readMaybe`.

> getEnvironment :: IO Environment
> getEnvironment = do
>   m <- lookupEnv "SCOTTY_ENV"
>   let e = case m of
>         Nothing -> Development
>         Just s -> read s
>   return e

We could've written this point-free.

    getEnvironment :: IO Environment
    getEnvironment = fmap (maybe Development read) (lookupEnv "SCOTTY_ENV")

Now that we've seen how to get the environment, let's see what the possible
environments are. You could add more environments, like `Staging`, to suite your
particular needs.

> data Environment
>   = Development
>   | Production
>   | Test
>   deriving (Eq, Read, Show)

With all the environment stuff out of the way, let's take a look at the database
connection pool. It will be used by the application to make database queries, so
it's responsible for configuring the database itself. That means logging,
connection parameters, and pool size. To start, the top-level function gets the
connection parameters and pool size, then determines which kind of logging to
use.

> getPool :: Environment -> IO DB.ConnectionPool
> getPool e = do
>   s <- getConnectionString e
>   let n = getConnectionSize e
>   case e of
>     Development -> runStdoutLoggingT (DB.createPostgresqlPool s n)
>     Production -> runStdoutLoggingT (DB.createPostgresqlPool s n)
>     Test -> runNoLoggingT (DB.createPostgresqlPool s n)

This function is a little weird. I wish it could be written like this:

    getPool :: Environment -> IO DB.ConnectionPool
    getPool e = do
      s <- getConnectionString e
      let n = getConnectionSize e
          p = DB.createPostgresqlPool s n
          t = case e of
            Development -> runStdoutLoggingT
            Production -> runStdoutLoggingT
            Test -> runNoLoggingT
      t p

Unfortunately the type system won't allow it. `runStdoutLoggingT` and
`runNoLoggingT` work on different monad transformers. `createPostgresqlPool` is
fine with either of them, but it can't accept both simultaneously.

Just like we looked up the environment through `SCOTTY_ENV`, we're going to look
up the database connection parameters through `DATABASE_URL`. It's expected to
look like this: `postgres://user:pass@host:port/db`. If it doesn't look like
that, we'll blow up.

    $ env DATABASE_URL=not-a-database-url cabal run
    hairy: couldn't parse absolute uri

If it's not given at all, we'll fall back to using a hard-coded default based on
the environment.

> getConnectionString :: Environment -> IO DB.ConnectionString
> getConnectionString e = do
>   m <- lookupEnv "DATABASE_URL"
>   let s = case m of
>         Nothing -> getDefaultConnectionString e
>         Just u -> createConnectionString (parseDatabaseUrl u)
>   return s

These are the default connection parameters per environment.

> getDefaultConnectionString :: Environment -> DB.ConnectionString
> getDefaultConnectionString Development =
>   "host=localhost port=5432 user=postgres dbname=hairy_development"
> getDefaultConnectionString Production =
>   "host=localhost port=5432 user=postgres dbname=hairy_production"
> getDefaultConnectionString Test =
>   "host=localhost port=5432 user=postgres dbname=hairy_test"

This function converts a list of text tuples into a database connection string,
which is a byte string. It joins each tuple with an equals sign and then joins
each element in the list with a space.

    > createConnectionString [("k1", "v1"), ("k2", "v2")]
    "k1=v1 k2=v2"

This is necessary to convert what `Web.Heroku.parseDatabaseUrl` gives us into
something that Persistent can understand.

> createConnectionString :: [(T.Text, T.Text)] -> DB.ConnectionString
> createConnectionString l =
>   let f (k, v) = T.concat [k, "=", v]
>   in  encodeUtf8 (T.unwords (map f l))

The last piece of the database puzzle is the size of the connection pool. In the
real world you'd need to benchmark performance using different sizes to see what
works best. A good baseline is two times the number of cores. That could be
expressed here using `GHC.Conc.numCapabilities`, but there's no guarantee that
the web server and the database server are even running on the same machine.

> getConnectionSize :: Environment -> Int
> getConnectionSize Development = 1
> getConnectionSize Production = 8
> getConnectionSize Test = 1

So we've set up our environment and our database connection. That's enough to
let us move on to setting up the application itself. All we need to do here is
get the options for Scotty and set up a runner for reading the configuration.

> runApplication :: Config -> IO ()
> runApplication c = do
>   o <- getOptions (environment c)

This takes Scotty's monad `m` and adds the ability to read our custom config `c`
from it. This is called a monad transformer stack. It allows us to use any monad
in the stack. So after layering on our config reader monad, we can both deal
with requests using Scotty's monad and read our config using our monad.

>   let r m = runReaderT (runConfigM m) c
>   scottyOptsT o r r application

Next we'll actually define our reader monad. This requires
`GeneralizedNewtypeDeriving` to easily and efficiently derive instances for our
type alias. The type signature of `runConfigM` tells us that it adds the ability
to read `Config` to the `IO` monad, which is the bottom of Scotty's monad
transformer stack.

> newtype ConfigM a = ConfigM
>  { runConfigM :: ReaderT Config IO a
>  } deriving (Applicative, Functor, Monad, MonadIO, MonadReader Config)

Let's circle back and see how we get Scotty's options. The data type exposed
only has two fields, so there's not a lot for us to do here.

> getOptions :: Environment -> IO Options
> getOptions e = do
>   s <- getSettings e
>   return def
>     { settings = s
>     , verbose = case e of
>       Development -> 1
>       Production -> 0
>       Test -> 0
>     }

I explicitly listed all of the environments here to ensure that I got all of
them. In the real world you might do something like this instead:

    verbose = case e of
      Development -> 1
      _ -> 0

Or, if you're feeling particularly witty:

    verbose = fromEnum (e == Development)

Most of the real options are in Wai's settings. The defaults are good for most
of them, but we want to make two changes. First, we need to remove the file
cache so that static file changes will be picked up. We only want to do this in
development since static files should be static in other environments. Then we
want to use the port in the `PORT` environment variable, if it's available.

> getSettings :: Environment -> IO Settings
> getSettings e = do
>   let s = defaultSettings

Here I'm using primes (`'`) to mark altered versions of the settings. There are
probably better ways to do this type of modification, but this works and is
straighforward.

>       s' = case e of
>         Development -> setFdCacheDuration 0 s
>         Production -> s
>         Test -> s
>   m <- getPort
>   let s'' = case m of
>         Nothing -> s'
>         Just p -> setPort p s'
>   return s''

Finally we need to handle looking up the port. Like our other functions that
read from environment variables, this one will blow up if you give it something
it's not expecting.

    $ env PORT=not-a-port cabal run
    hairy: Prelude.read: no parse

> getPort :: IO (Maybe Int)
> getPort = do
>   m <- lookupEnv "PORT"
>   let p = case m of
>         Nothing -> Nothing
>         Just s -> Just (read s)
>   return p

The last bit of configuration is to set up our error type. We're going to make
it an alias for `Text`. You could do something fancier here by enumerating the
possible error states for your application.

    data Error = NotFoundError | ForbiddenError | ...
    instance ScottyError Error where ...

We're alright with the default textual errors, so we don't need anything that
fancy yet.

> type Error = Text

That wraps up all of the configuration, options, and settings. Everything from
here on out deals with the application itself.

Our application has several responsibilities. It needs to run database
migrations, set up middlewares, install a default exception handler, and define
routes. Since everything else could conceivably depend on the database, we'll
run the migrations first.

> application :: ScottyT Error ConfigM ()
> application = do
>   runDB (DB.runMigration migrateAll)

`runDB` is a utility function we'll define a little later. It basically lifts a
database operation into the current Scotty monad. `migrateAll` comes from
`Hairy.Models` and is generated by Persistent using Template Haskell.

Now that the database has been migrated, we can set up middlewares and exception
handlers. Both of them depend on the environment, so we have to get that from
our config reader monad first.

>   e <- lift (asks environment)
>   middleware (loggingM e)
>   defaultHandler (defaultH e)

Finally we can do the routing for our application. All we need is the HTTP
method, the path, and the action to route it to.

>   get "/tasks" getTasksA
>   post "/tasks" postTasksA
>   get "/tasks/:id" getTaskA
>   put "/tasks/:id" putTaskA
>   delete "/tasks/:id" deleteTaskA

Routes are matched top down, so if nothing else matched we'll render our not
found action.

>   notFound notFoundA

That's it! As your application grows you'll add more routes and middlewares, but
the basic structure shouldn't change too much.

Let's take a look at that `runDB` helper we used. It takes a SQL query `q` and
runs it inside our monad transformer stack. It does this by asking the config
reader for the database connection pool, then running the query with that pool
in the IO monad.

> runDB :: (MonadTrans t, MonadIO (t ConfigM)) =>
>   DB.SqlPersistT IO a -> t ConfigM a
> runDB q = do
>   p <- lift (asks pool)
>   liftIO (DB.runSqlPool q p)

Up next is the logging middleware. In development we want colorful multiline
logs flushed every request. In production we want plain log lines flushed
sometimes. In testing we don't want logging at all.

> loggingM :: Environment -> Middleware
> loggingM Development = logStdoutDev
> loggingM Production = logStdout
> loggingM Test = id

Before we define our default exception handler, let's create an alias for our
Scotty actions. They're all going to have the same type, so we don't want to
repeat ourselves over and over again.

> type Action = ActionT Error ConfigM ()

Since our default exception handler handles uncaught exceptions in our
application, we want it print out the exceptions in development but swallow them
in production (we don't really care what happens to them in testing). In the
real world you might send the exception to another service.

> defaultH :: Environment -> Error -> Action
> defaultH e x = do
>   status internalServerError500
>   let o = case e of
>         Development -> object ["error" .= showError x]
>         Production -> Null
>         Test -> object ["error" .= showError x]
>   json o

At long last we can get to the meat of our application: the actions. This is
where all of your business logic lives. Since Hairy is just a basic CRUD app,
there's not a lot going on here. This action gets all the tasks from the
database and renders them as JSON.

> getTasksA :: Action
> getTasksA = do
>   ts <- runDB (DB.selectList [] [])
>   json (ts :: [DB.Entity Task])

This one allows you to create new tasks by posting JSON to it. If the JSON isn't
valid, an exception will be raised. That means in development you'll get a
helpful error message, but in production you'll get a blank 500.

    $ curl -X POST localhost:3000/tasks -d 'not valid json'
    {"error":"jsonData - no parse: not valid json"}

> postTasksA :: Action
> postTasksA = do
>   t <- jsonData
>   runDB (DB.insert_ t)
>   status created201
>   json (t :: Task)

This action gets a task from the database. If it was found, it renders it as
JSON. If it wasn't, it renders the generic not found action.

> getTaskA :: Action
> getTaskA = do
>   i <- param "id"
>   m <- runDB (DB.get (toKey i))
>   case m of
>     Nothing -> notFoundA
>     Just t -> json (t :: Task)

This one will either update an existing task or create a new one with the given
ID. Then it renders the task as JSON.

> putTaskA :: Action
> putTaskA = do
>   i <- param "id"
>   t <- jsonData
>   runDB (DB.repsert (toKey i) t)
>   json (t :: Task)

This is the last action. It will delete a task with the given ID. If there is no
such task, it returns 200 anyway. In either case, `null` is returned.

> deleteTaskA :: Action
> deleteTaskA = do
>   i <- param "id"
>   runDB (DB.delete (toKey i :: TaskId))
>   json Null

That wraps up the business logic. We only have a couple things to attend to. We
used `toKey`, a helper function that converts a request parameter into a
database key. It allows us to query for stuff from the database using request
parameters.

This helper function requires the FlexibleContexts language extension, although
I can't really tell you why. If you don't have it, GHC complains. If you do have
it, everything works fine.

> toKey :: DB.ToBackendKey DB.SqlBackend a => Integer -> DB.Key a
> toKey i = DB.toSqlKey (fromIntegral (i :: Integer))

The last thing we need to do is define our not found action. All it does is set
the HTTP status to 404 and render `null`.

> notFoundA :: Action
> notFoundA = do
>   status notFound404
>   json Null

That's all there is to it! With less than 200 lines of code we've created a JSON
REST API with some CRUD actions. It's all backed by a database and can be
configured to run in development mode on your machine or in production on
Heroku.
