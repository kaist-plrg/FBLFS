## Getting Started

## Prerequisites

- Linux
```sh
apt install curl make gcc unzip bubblewrap libffi-dev libgmp-dev pkg-config
```

- MacOS
```sh
brew install libffi-dev gmp pkg-config
```

## Java
Requires java 17 or higher. Make sure that the `java` command is available in the terminal.


## Build

```sh
$ ./install.sh
$ eval $(opam env)
$ dune build
```