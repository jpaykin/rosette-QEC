# LER

A  playground for using [Rosette](https://emina.github.io/rosette/) (Racket's solver-aided
language) for estimating the logical error rate (LER) of a quantum error-correcting code.

## Setup

1. Install Racket: [Download](https://download.racket-lang.org/)

    - MacOS: `brew install --cask racket`

2. Install Rosette: `raco pkg install --auto rosette`

## Run

```sh
racket main.rkt
```


## Files

- `info.rkt` — package manifest (name `LER`, deps on `base` and `rosette`)
- `main.rkt` — sketch + spec + synthesis query, all in one file
