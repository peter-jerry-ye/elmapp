# elmapp

An functional and compositional approach to web applications via bidirectional transformation.

## Build the project

In order to build the project, `cabal` and `nix` are necessary.

Since the project is based on [`miso`](https://github.com/dmjio/miso) library, we also need to use the cachix for miso with following code:
```bash
nix-env -iA cachix -f https://cachix.org/api/v1/install
cachix use miso-haskell
```

Then we can install the project:
- Clone the project
- Execute `nix-shell` to enter the development environment
- Execute `cabal build app --ghcjs` to build the task management application
- Open `app.html` to see the result.

## Project Structure

- `Elmlens.hs` defines the ElmApp with combinators.
- `Apps.hs` defines some simple examples.
- `Todo.hs` defines the themed task management application.
