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
- Set up the Haskell Language Server  
- Using Debug.Trace to debug print  
- [Reading environment variable](#Reading-environment-variable) and [making it accessible to your app](#Making-it-accessible-to-your-app) 
- [Setting up the server](#Setting-up-the-server).
  - [Creating your first route](#First-route)
  - [Creating the API Type](#API-Type)
  - [Creating the application](#Application)
  - [Testing the API with Postman](#Testing-the-API-with-Postman)
  - [Setting up the client authentication using JWT](#Setting-up-the-client-authentication-using-JWT).
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
├── Haskell-API-Tutorial.cabal  -- The project's cabal configuration. Includes dependencies and such.
├── LICENSE
├── README.md
├── src          -- Library source code
│   └── ...
├── test         -- Library tests
    └── ...
└── ...
```
---
# The Tutorial
---
## Reading environment variable
###### _see [src/Env.hs](src/Env.hs)_  
  
We import `System.Environment` to read env variables from the machine running the API.  
We declare a `Env` data record to hold our variables and a `initEnv` function to create it.  
That way:
``` haskell
{-# LANGUAGE RecordWildCards #-}

module Env where
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
  
We create an instance of `Env` using `initEnv` and pass it to our function inside a `MonadReader` thanks to `runReaderT`.  
`printMsgFromEnv` can then query the `startingMessage` field by using `asks`.  
    
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
> Notice how `printMsgFromEnv` is of type `m ()` instead of `m (IO ())` even though we are calling `putStrLn`.  
> This is made possible thanks to the constraint `MonadIO`.
> 
> `MonadIO` warns about the same _"side-effects"_ as `IO` but without altering the return type,   
> `liftIO` then "translate" a `IO _` to a `MonadIO m => m _`.  
> This is necessary beacuse `IO ()` is not supported by `runReaderT` but `MonadIO m => m ()` is. 

---
## Setting up the server

We are using [`Servant`](https://docs.servant.dev/en/stable/tutorial/install.html) to _"create an abstract web app" (quoted from Servant's doc)_, and [`Wai`](https://hackage.haskell.org/package/wai) to serve it.

To construct an `Application` that `Wai` can serve, `Servant` needs a `type` that defines all our routes.  
So, let's start with...

### First route 
###### see _[src/Api/Routes/HelloWorld.hs](src/Api/Routes/HelloWorld.hs)_.  

Using functions provided by `Servant`, we declare our `HelloWorld` route as a `type`.  

This route, accessible at `"/hello-world"`, on `GET` request, will return a `JSON` body matching the type `HelloWorldResponse`.  
The `ToJSON` instance of `HelloWorldResponse` will allow `Servant` to convert the data record into a json the way we want.  

We also declare the `helloWorldFunction` to handle requests made on that route. 

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

module Api.Routes.HelloWorld where

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

### API Type
###### see _src/Api/Api.hs_.  

We simply declare the `API` `type` and assign our `HelloWorld` route.  

``` Haskell
module Api.Api where 
import qualified Api.Routes.HelloWorld as Routes (HelloWorld)

type Api = Routes.HelloWorld
-- Add more endpoint using ":<|>"
```

Now, we can use that type to create the `Application` and the server:

### Application
###### see _src/Api/Server.hs_.

We declare a `server` function of type `Server Api`. We then simply return our route's function.

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
-- Like so:
--      :  <|> Routes.NuclearBombLauncher

appM :: (MonadIO m) => m Wai.Application 
appM = Servant.serve (Proxy :: Proxy Api) <$> server
```
> [!IMPORTANT]
> All of these functions run under `MonadIO` _(and other constraints like `MonadReader Env` depending on your needs)_.  
> No constraint are mandatory to run an api.  
> `MonadIO` is included as a example of how to use constraints in your routes.      


Finally, we update `main` to run our `Application`:

``` Haskell
import Network.Wai.Handler.Warp (run)

import Env (Env, startingMessage, initEnv)
import Api.Server (appM)

main :: IO ()
main = do
  env <- initEnv
  app <- appM
  runReaderT (liftIO $ run 8080 app) env
```

Run `cabal build && cabal run` and your API should be working !  

### Testing the API with Postman 

![image](https://github.com/user-attachments/assets/eb53b71f-a0e6-4eea-826b-a6681b687899)

🎉 If you see a json similar to that one, it means your API is working properly ! 🎉  
🎉 Congratulation on creating your first route ! 🎉

> [!TIP]
> Postman is the easiest way to debug an api, and it's free.

---

## Setting up the client authentication using JWT 

> [!NOTE]
> Understanding [Json Web Token (JWT)](https://en.wikipedia.org/wiki/JSON_Web_Token) would be helpful.  
> In short: a JWT stores one of your user's encrypted data.  
> Verifying this token allows the API to understand which user is issuing a request and if it loged in previously.

> [!TIP]
> Postman will be very helpful here.

> [!IMPORTANT]
> We wil rework most of what we have done to make the code cleaner.  
> Consider the previous steps as a basic introduction to Servant.
> Finally, our API will set the JWT inside a Cookie. 

Here are the steps we will folow:  
1. Create a `login` route to generate a JWT for your user
2. Create a protected route that requires this JWT to access
3. Update the Api type
4. Update the server and application

### Creating a type for our JWT  
###### _see [Login.hs](https://github.com/Julian52575/Haskell-Servant-API-Tutorial/blob/5-server/src/Api/JWTPayload.hs)_  

Any type will do, as long as it implement instances of `FromJSON`, `ToJSON`, `FromJWT` and `ToJWT`.  
``` haskell
data JWTPayload = JWTPayload {
    userName :: Text
} deriving (Eq, Show, Generic)

instance Aeson.FromJSON         JWTPayload
instance Aeson.ToJSON           JWTPayload
instance Servant.Auth.FromJWT   JWTPayload
instance Servant.Auth.ToJWT     JWTPayload
```
This example type will only share the user name between our API and our client.  

> [!TIP]
> If you don't understand why we call the type `Payload`, maybe you should learn about JWT

### Creating a login route to generate our client's JWT
###### _see [Login.hs](https://github.com/Julian52575/Haskell-Servant-API-Tutorial/blob/5-server/src/Api/Routes/Login.hs)_  

Let's first declare our route type:  
``` haskell
{-# LANGUAGE DeriveGeneric #-}
-- ^ For TokenResponse.deriving Generic
{-# LANGUAGE NamedFieldPuns #-}
-- ^ For HelloWorldResponse{message}
{-# LANGUAGE RecordWildCards #-}
-- ^ For HelloWorldResponse{..}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
-- ^ For type HelloWorld = "hello-world" :>
{-# LANGUAGE OverloadedStrings #-}
-- ^ For o .: String

module Api.Routes.Login where
import Servant ((:>), JSON, ReqBody, Post, throwError, err500, Handler, Header, Headers, err401, NoContent(..))
import Servant.Auth.Server (makeJWT, JWTSettings, AuthResult, SetCookie, acceptLogin, defaultCookieSettings)

data LoginBody = LoginBody {
    loginName :: Text,
    loginPassword :: Text
} deriving (Generic)
instance Aeson.FromJSON LoginBody

type Login = "login"
            :> ReqBody '[JSON] LoginBody
            :> Post '[JSON] (Headers '[Header "Set-Cookie" SetCookie, Header "Set-Cookie" SetCookie] NoContent)
```
You should understand what the `LoginBody` does, but in case you don't, it's the json object that needs to be passed inside a request.  
We define a default instance _(`instance Aeson.FromJSON LoginBody`)_, so the json keys will match the record names.  
Here is an example of the json object:  
```json
{
  "loginName" : "Rulian",
  "loginPassword" : "IluvHaskell"
}
```
Now, back to the route type:  
Notice the return type ```Headers '[Header "Set-Cookie" SetCookie, Header "Set-Cookie" SetCookie] NoContent```,  
it means we will return a `NoContent` **AND** set 2 cookies (`[Header "Set-Cookie" SetCookie, Header "Set-Cookie" SetCookie]`).  
How do we do that ?
- We'll return `NoContent`.
- Let Servant's [acceptLogin](https://hackage.haskell.org/package/servant-auth-server-0.4.9.0/docs/Servant-Auth-Server.html#v:acceptLogin) manage the cookies !  
acceptLogin takes 3 arguments: a `JWTSettings`, a `CookieSettings` and `session`, which is whatever type we want to encrypt within our JWT.     
It returns a `IO (Maybe (response -> withTwoCookies))`, which bind whatever our `response` type is to a `response` with two cookies.  
Now that you understand our operations, here is the login function:  
``` haskell
loginFunction :: JWTSettings -> LoginBody -> Handler (Headers '[Header "Set-Cookie" SetCookie, Header "Set-Cookie" SetCookie] NoContent)
loginFunction jwtCfg (LoginBody name _password) = do
  let payload = JWTPayload { userName = name }
  loginAcceptMaybe <- liftIO $ acceptLogin defaultCookieSettings jwtCfg payload
  case loginAcceptMaybe of
    Nothing -> throwError err401
    Just responseToCookieFun -> do
      let response = NoContent
      return $ responseToCookieFun response
```
We parse the LoginBody to a JWTPayload that will be encoded and stored within the cookies that `acceptLogin` will return.  
