# QEC Analysis

A library for analyzing properties of quantum error-correcting codes using
symbolic executionj via [Rosette](https://emina.github.io/rosette/) (Racket's solver-aided language).

## Setup

1. Install Racket: [Download](https://download.racket-lang.org/)

    - MacOS: `brew install --cask racket`

## Run the demo

```sh
racket main.rkt
```

The `qec` collection exposes the public operations of all three domains without
running the demo:

```racket
#lang racket
(require qec)

(pretty-pauli (symbols->pauli '(X Z)))
```

For a narrower API, require `qec/src/paulis`, `qec/src/stabilizer-codes`, or
`qec/src/circuits`.
Examples are available separately, for example `(require qec/src/examples/toric-code)`.

## Test

```sh
raco test tests/core.rkt tests/circuit.rkt tests/interfaces.rkt
```

## Clifford circuits

Circuits are Racket values with zero-based qubit indices. Qubits start in
`|0>`. Instructions are executed in list order:

```racket
#lang rosette
(require qec)

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
`src/examples/repetition-circuit.rkt` for a three-qubit example.

This is a circuit API, not a Stim parser. Noise probabilities, multi-qubit
fault gates, sampling, solver-based fault search, feedback, gauge detectors,
and Pauli-target observables are not implemented.

## Project layout

- `info.rkt` — package metadata and dependencies
- `main.rkt` — aggregate public interface and demo entry point
- `src/paulis.rkt`, `src/stabilizer-codes.rkt`, `src/circuits.rkt` — public interfaces
- `src/paulis/` — Pauli definitions, algebra, and group sampling
- `src/stabilizer-codes/` — logical-error predicates, synthesis, and sampling
- `src/circuits/` — circuit evaluation and ideal stabilizer reference
- `src/examples/` — code and circuit examples
- `src/demo.rkt` — demonstration routine
- `tests/core.rkt` — focused RackUnit tests
