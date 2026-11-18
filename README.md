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
raco test tests/core.rkt tests/circuit.rkt
```

## Clifford circuits

Circuits are Racket values with zero-based qubit indices. Qubits start in
`|0>`. Instructions are executed in list order:

```racket
#lang rosette
(require LER)

(define-symbolic x z boolean?)
(define result
  (run-circuit
   (circuit 1
            (list (reset-qubit 0 'Z)
                  (gate-fault 0 (pauli1 x z))
                  (measure-qubit 0 'Z)
                  (detector '(-1))
                  (observable 0 '(-1))))))

(circuit-result-detectors result)       ; (list x)
(circuit-result-observables result)     ; (list x)
(undetected-logical-error? result)     ; false: the detector also flips
```

`gate-h`, `gate-s`, and `gate-cx` apply H, S, and controlled-X; CX takes
control then target. `gate-pauli` applies an intentional single-qubit `pauli1`,
whereas `gate-fault` applies a fault relative to an identity operation. Both
accept concrete or symbolic Boolean Pauli exponents, e.g. `pauli1X` or
`(pauli1 x z)`. No separate fault assignment or solver is required.
`measure-qubit` and `reset-qubit` take a qubit and one of `'X`, `'Y`, or `'Z`;
compose them for measure-and-reset. A reset does not append a measurement.

Detectors XOR their measurement-record offsets, where `-1` is the latest
measurement at that instruction. Observable annotations similarly XOR record
offsets into the specified nonnegative ID, including across multiple
annotations. Repeated offsets cancel. Results contain detector flips in
instruction order and observable flips in ID order, with gaps represented by
`#f`. Flips are relative to the fault-free circuit, not raw measurement
values. An undetected logical error has no detector flips and at least one
observable flip.

`run-circuit` rejects invalid instructions or references, nondeterministic
detector parities, and nondeterministic completed observable parities. Individual
measurements may be random when their annotated parity is deterministic.
Symbolic intentional Pauli gates are part of the ideal reference; fault gates
are replaced by identity in that reference. See
`src/ler/examples/repetition-circuit.rkt` for a three-qubit example.

This is a circuit API, not a Stim parser. Noise probabilities, multi-qubit
fault gates, sampling, solver-based fault search, feedback, gauge detectors,
and Pauli-target observables are not implemented.

## Project layout

- `info.rkt` — package metadata and dependencies
- `main.rkt` — public facade and demo entry point
- `src/ler/pauli.rkt` — Pauli representation, conversion, and display
- `src/ler/algebra.rkt` — commutation, multiplication, and generator operations
- `src/ler/logical.rkt` — logical-error predicates and synthesis
- `src/ler/circuit.rkt` — circuit instructions and fault-frame evaluation
- `src/ler/circuit-reference.rkt` — ideal stabilizer reference and annotation validation
- `src/ler/examples/repetition-circuit.rkt` — circuit-based repetition-code example
- `src/ler/sampling.rkt` — group and logical-error sampling
- `src/ler/demo.rkt` — demonstration routine
- `tests/core.rkt` — focused RackUnit tests
