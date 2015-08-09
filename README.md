# todobackend-haskell

[![build status](https://circleci.com/gh/jhedev/todobackend-haskell.svg?style=shield)](https://circleci.com/gh/jhedev/todobackend-haskell/tree/master)

This repository provides different Haskell implementations for [todobackend](http://www.todobackend.com/).

The [`todobackend-common`](https://github.com/jhedev/todobackend-haskell/tree/master/todobackend-common) package
implements common functionality, such as the model and some utils.


### Docker images

To run the docker container of the scotty implementation:

```
docker run --rm -it -p 3000:3000 jhedev/todobackend-haskell:scotty 
```

The application is now running on port 3000. For any other implementation just replace `scotty` with `servant`, `snap`, `spock` or `yesod`.
