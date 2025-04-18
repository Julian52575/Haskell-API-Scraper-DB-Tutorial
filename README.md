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
- [Reading environment variable and making it accessible to your app](#Reading-environment-variable) 
- Creating a server to interact with your clients:
  - Setting up the server.
  - Setting up a route.
  - Replying to the client's request.
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
_see src/Env.hs_  
We import `System.Environment` to read env variables from the machine running the API.  
We declare a `Env` data record to hold our variables and a `initEnv` function to create it.
Like that:
``` haskell
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


### Make it accessible to your app

