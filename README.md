# Haskell-API-Tutorial
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
Haskell makes both fast and very-realible APIs.

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
- Reading environment variable and make it accessible to your app.
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
