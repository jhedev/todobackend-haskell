# todobackend-haskell

Six Haskell implementations of [Todo-Backend](https://www.todobackend.com/), a shared spec for a small JSON API that manages a todo list. Each backend implements the same API with a different web framework, so you can compare them side by side.

> **Note:** The public demos that ran on sloppy.io are no longer online. To try a backend, build and run it locally as described below.

## Implementations

| Package | Framework | Source |
| --- | --- | --- |
| `todobackend-happstack` | [Happstack](https://www.happstack.com/) | [`Main.hs`](todobackend-happstack/src/Main.hs) |
| `todobackend-scotty` | [Scotty](https://hackage.haskell.org/package/scotty) | [`Main.hs`](todobackend-scotty/src/Main.hs) |
| `todobackend-servant` | [Servant](https://www.servant.dev/) | [`Main.hs`](todobackend-servant/src/Main.hs) |
| `todobackend-snap` | [Snap](http://snapframework.com/) | [`Main.hs`](todobackend-snap/src/Main.hs) |
| `todobackend-spock` | [Spock](https://hackage.haskell.org/package/Spock-core) | [`Main.hs`](todobackend-spock/src/Main.hs) |
| `todobackend-yesod` | [Yesod](https://www.yesodweb.com/) | [`Main.hs`](todobackend-yesod/src/Main.hs) |

All six depend on [`todobackend-common`](todobackend-common), which holds the code they share:

- [`TodoBackend.Model`](todobackend-common/src/TodoBackend/Model.hs): the `Todo` entity (Persistent), its JSON request and response types, and `runDb`, which runs queries against SQLite.
- [`TodoBackend.Utils`](todobackend-common/src/TodoBackend/Utils.hs): WAI middleware for CORS headers and `OPTIONS` requests.

## Getting started

### Prerequisites

- [Stack](https://docs.haskellstack.org/). The project uses the `lts-24.67` resolver (GHC 9.10.3); Stack installs that GHC for you.
- Git. Stack fetches `Spock-core` from the upstream repository, because the release on Hackage does not build with GHC 9.6 or newer.

### Build

```sh
stack build                       # all packages
stack build todobackend-scotty    # or just one
```

The first build compiles every dependency and takes a while.

### Run

Every backend reads two required environment variables:

| Variable | Meaning |
| --- | --- |
| `PORT` | Port the server listens on. |
| `URL` | Public base URL of the server, used to build each todo's `url` field. |

For example, to run the Scotty backend on port 3000:

```sh
PORT=3000 URL=http://localhost:3000 stack exec todobackend-scotty
```

Replace `scotty` with `happstack`, `servant`, `snap`, `spock` or `yesod` to run another implementation.

Todos are stored in `dev.sqlite3` in the current working directory. The database and its table are created on startup if they do not exist.

## API

| Method | Path | Description |
| --- | --- | --- |
| `GET` | `/todos` | List all todos. |
| `POST` | `/todos` | Create a todo. |
| `DELETE` | `/todos` | Delete all todos. |
| `GET` | `/todos/:id` | Get one todo. |
| `PATCH` | `/todos/:id` | Update some fields of a todo. |
| `DELETE` | `/todos/:id` | Delete one todo. |

Requests take a JSON body with any of `title`, `completed` and `order`. When creating a todo, missing fields default to `""`, `false` and `0`. Responses also include the todo's `id` and `url`:

```sh
$ curl -X POST http://localhost:3000/todos \
    -H 'Content-Type: application/json' \
    -d '{"title": "walk the dog"}'
{"id":1,"url":"http://localhost:3000/todos/1","title":"walk the dog","completed":false,"order":0}
```

All responses allow cross-origin requests, so browser clients on other origins can call the API.

## Testing against the spec

The repository has no test suite of its own. Use the official Todo-Backend tools instead, with a backend running locally:

- **Spec runner:** [https://www.todobackend.com/specs/index.html?http://localhost:3000/todos](https://www.todobackend.com/specs/index.html?http://localhost:3000/todos)
- **Reference client:** [https://www.todobackend.com/client/index.html?http://localhost:3000/todos](https://www.todobackend.com/client/index.html?http://localhost:3000/todos)

## Repository layout

```text
.
├── todobackend-common/     # shared model, JSON types and middleware
├── todobackend-happstack/  # one package per framework
├── todobackend-scotty/
├── todobackend-servant/
├── todobackend-snap/
├── todobackend-spock/
├── todobackend-yesod/
├── stack.yaml              # resolver and extra dependencies
├── stack.yaml.lock
└── .agents/setup           # prepares an Amp orb to build the project
```

## License

MIT. See the `LICENSE` file in each package, for example [`todobackend-common/LICENSE`](todobackend-common/LICENSE).
