# LER

A  playground for using [Rosette](https://emina.github.io/rosette/) (Racket's solver-aided
language) for estimating the logical error rate (LER) of a quantum error-correcting code.

## Setup

1. Install Racket: [Download](https://download.racket-lang.org/)

    - MacOS: `brew install --cask racket`

2. Install the package and its dependencies (replace the path with the cloned
   project directory):

    ```sh
    raco pkg install --auto --link /path/to/rosette-QEC
    ```

## Run the demo

```sh
racket main.rkt
```

The reusable implementation is split into modules under `src/ler/`. Requiring
the `LER` collection makes their public operations available without running
the demo:

```racket
#lang racket
(require LER)

(pretty-pauli (symbols->pauli '(X Z)))
```

## Test

```sh
raco test tests/core.rkt
```

## Project layout

- `info.rkt` — package metadata and dependencies
- `main.rkt` — public facade and demo entry point
- `src/ler/pauli.rkt` — Pauli representation, conversion, and display
- `src/ler/algebra.rkt` — commutation, multiplication, and generator operations
- `src/ler/logical.rkt` — logical-error predicates and synthesis
- `src/ler/sampling.rkt` — group and logical-error sampling
- `src/ler/demo.rkt` — demonstration routine
- `tests/core.rkt` — focused RackUnit tests
