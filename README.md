# Haskell API Tutorial
Learn how to build your own haskell REST API.  
Feel free to jump arround from task to task and use this repository as a reference for your own project ~I know ChatGPT will~.

## FAQ
### What is a [rest api](https://www.redhat.com/en/topics/api/what-is-a-rest-api) ?
---
### What is [Haskell](https://www.haskell.org/) ?
Haskell is a programming language that is:
- Functional
- Lazy: Expressions are highly optimized and only computed if they are needed.
- Pure: Operations that might produce a side-effect are encapsulated within a 'Monad' _(For example: Input/Output operation to read or write)_.
  Making crashes very rare.
- Optimized: Comparable to C or C++.

### Why haskell ?
Haskell is both fast and very-reliable, making it perfect for web services such as APIs.

---
## Dependencies
- [Ghcup](https://www.haskell.org/ghcup/)
- Cabal >= 3.0
> [!TIP]
> Run `ghcup tui` to easily install the correct version of Cabal.
- [Postman](https://www.postman.com/downloads/) _(optional)_
> [!TIP]
> `Postman` will help you test your routes !

## What you will learn
- [Reading environment variable](#Reading-environment-variable) and [making it accessible to your app](#Making-it-accessible-to-your-app) 
- [Setting up the server](#Setting-up-the-server).
  - [Creating our first route](#First-route)
  - [Creating the API Type](#API-Type)
  - [Creating the application](#Application)
  - [Testing the API with Postman](#Testing-the-API-with-Postman)
  - Setting up the client authentication.
- Querying a 3rd party API:
  - Sending a request.
  - Handle the response and status code.
- Interacting with a database:
  - Connecting.
  - Reading data.
  - Writing data.

## Project structure

```
.
├── app         -- Application source code
│   └── Main.hs
|   └── ...     
├── CHANGELOG.md
├── Haskell-API-Tutorial.cabal  -- The project's configuration file
├── LICENSE
├── README.md
├── src          -- Library source code
│   └── MyLib.hs
├── test         -- Library tests
    └── Main.hs
└── ...
```
---
# The Tutorial
---
## Reading environment variable
###### _see [src/Env.hs](src/Env.hs)_  
  
We import `System.Environment` to read env variables from the machine running the API.  
We declare a `Env` data record to hold our variables and a `initEnv` function to create it.
Like that:
``` haskell
{-# LANGUAGE RecordWildCards #-}
import System.Environment (getEnv)

data Env = Env {
    startingMessage :: Text
    , apiPort :: Int
    , databaseIp :: IPv4
    , databaseTable :: Text
} deriving (Show)

initEnv :: IO Env
initEnv = do
    startingMessage <- pack <$> getEnv "STARTING_MESSAGE"
    apiPort <- read <$> getEnv "API_PORT"
    databaseIp <- read <$> getEnv "DATABASE_IP"
    databaseTable <- pack <$> getEnv "DATABASE_TABLE"
    pure Env{..}
```
> [!Tip]
> We are using the `RecordWildCards` language extension to automatically pass each member of Env using `{..}` as they are defined in the function scope.

## Making it accessible to your app
###### _see [app/Main.hs](app/Main.hs) and [src/Env.hs](src/Env.hs)_  
  
We create a instance of `Env` using `initEnv` and pass it to our function inside a `MonadReader` thanks to `runReaderT`.  
`printMsgFromEnv` can query the `startingMessage` field of the data record `Env`.  
    
**Why `MonadReader` instead of passing `Env` as a parameter ?**  
`MonadReader` acts as an environment of its own, holding our `Env` record.  
If `printMsgFromEnv` were to call `another` function that has the `MonadReader Env` constraint, `another` would have access to `Env` the same way without having to pass the parameter again.

``` haskell
import Control.Monad.Reader.Class (MonadReader, asks)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Reader (runReaderT)

import Env (Env, startingMessage, initEnv)

printMsgFromEnv :: (MonadReader Env m, MonadIO m) => m ()
printMsgFromEnv = do
  msgRaw <- asks startingMessage
  liftIO $ putStrLn (unpack msgRaw)

main :: IO ()
main = do
  env <- initEnv
  runReaderT printMsgFromEnv env
```
> [!Tip]
> Notice how `printMsgFromEnv` is of type `m ()` instead of `IO ()` even though we are calling `putStrLn`.  
> This is made possible thanks to the constraint `MonadIO`.
> 
> `MonadIO` warns about the same "side-effects" as `IO` but without altering the return type,   
> `liftIO` then "translate" a `IO _` to a `MonadIO m => m _`.  
> This is necessary beacuse `IO ()` is not supported by `runReaderT` but `MonadIO m => m ()` is. 

---
## Setting up the server

We are using [`Servant`](https://docs.servant.dev/en/stable/tutorial/install.html) to "create an abstract web app" _(quoted from Servant's doc)_,    
and [`Wai`](https://hackage.haskell.org/package/wai) to serve it.

To construct a `Wai` `Application`, Servant needs:
- A `type` that transform all our routes into a singular `API` type.  
- Other `types` for each of our routes.  
- A proxy.

To run an `Application`, Wai needs:
- A port.

Contrary to other language, we must first define the routes and then the `API` `type` before declaring the server.    
So, let's start with...

---
### First route 
###### see _[src/Api/Routes/HelloWorld.hs](src/Api/Routes/HelloWorld.hs)_.  

Using the `TypeOperators` language extension and functions provided by `Servant`, we declare our `HelloWorld` route as a `type`.  

This route, accessible at `"/hello-world"`, on `GET` request, will return a `JSON` body matching the type `HelloWorldResponse`. The `ToJSON` instance of `HelloWorldResponse` will allow `Servant` to convert the data record into a json the way we want.  

We also declare the `helloWorldFunction` to handle request made on that route. 

``` Haskell
{-# LANGUAGE NamedFieldPuns #-}
-- ^ For HelloWorldResponse{message}
{-# LANGUAGE RecordWildCards #-}
-- ^ For HelloWorldResponse{..}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
-- ^ For type HelloWorld = "hello-world" :>
{-# LANGUAGE OverloadedStrings #-}
-- ^ For o .: String

import Data.Aeson
import Data.UnixTime (UnixTime, getUnixTime)
import Servant ((:>), Get, JSON)
import Control.Monad.IO.Class (MonadIO, liftIO)

type HelloWorld = "hello-world" :> Get '[JSON] HelloWorldResponse 

data HelloWorldResponse = HelloWorldResponse {
    message :: String,
    time :: UnixTime
} deriving (Show)

instance ToJSON HelloWorldResponse where
    toJSON HelloWorldResponse{message, time} = object
        [
            "message" .= message,
            "time" .= utSeconds time
        ]

helloWorldFunction :: (MonadIO m) => m HelloWorldResponse
helloWorldFunction = do
    let message = "Hello World !"
    time <- liftIO getUnixTime
    pure HelloWorldResponse{..}
```

> [!Tip]
> Notice that we are again using `MonadIO` and `liftIO` for our `UnixTime`.  

> [!Tip]
> Notice that we are also using `{..}` from the `RecordWildCard` language extension again.  
> It's very helpful !

We can now create the...

---
### API Type
###### see _src/Api/Api.hs_.  

We simply declare the `API` `type` and assign our `HelloWorld` route.  

``` Haskell
import qualified Api.Routes.HelloWorld as Routes (HelloWorld)

type Api = Routes.HelloWorld
-- Add more endpoint using ":<|>"
```

and finish with the...

---
### Application
###### see _src/Api/Server.hs_.

We declare a `server` function returning a `Server Api`. We then simply return our route's function.

``` Haskell
import Data.Proxy 
import qualified Servant (Server, serve)
import qualified Network.Wai as Wai (Application)
import Control.Monad.IO.Class (MonadIO)

import Api.Api (Api)
import qualified Api.Routes.HelloWorld as Routes (helloWorldFunction)

server :: (MonadIO m) => m (Servant.Server Api)
server = return Routes.helloWorldFunction
-- Use Servant's ":<|>" operator to add other route's function. Make sure it matches the order set in the Api type.

appM :: (MonadIO m) => m Wai.Application 
appM = Servant.serve (Proxy :: Proxy Api) <$> server
```
> [!IMPORTANT]
> All of these functions run under `MonadIO` _(and other constraints like `MonadReader Env` depending on your needs)_.  
> No constraint are mandatory to run an api.  
> `MonadIO` is included as a example of how to use constraints in your routes.      

---

Finally, we update `main` to run our `Application`:

``` Haskell
import Network.Wai.Handler.Warp (run)

import Env (Env, startingMessage, initEnv)
import Api.Server (appM)

main :: IO ()
main = do
  env <- initEnv
  app <- appM
  run 8080 app
```

Run `cabal build && cabal run` and your API should be working !  

### Testing the API with Postman 

![image](https://github.com/user-attachments/assets/eb53b71f-a0e6-4eea-826b-a6681b687899)

🎉 If you see a json similar to that one, it means your API is working properly ! 🎉  
🎉 Congratulation on creating your first route ! 🎉

> [!TIP]
> Postman is the easiest way to debug a api, and it's free.

---
