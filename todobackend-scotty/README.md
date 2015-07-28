todobackend-scotty-persistent
=============================
[![build status](https://circleci.com/gh/jhedev/todobackend-scotty-persistent.svg?style=shield)](https://circleci.com/gh/jhedev/todobackend-scotty-persistent/tree/master)


Todobackend written in Haskell using Scotty and Persistent libraries

###Build and run locally
Clone the repository and run the following commands:

```
cabal sandbox init
cabal install --only-dep
PORT=3000 cabal run
```

The application is now running on port 3000.
