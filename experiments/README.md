# Experiment 2: Sampling All 15 Logical Classes

**File:** `sample-logical-weights-all.rkt`

This experiment samples physical Pauli operators from all 15 nontrivial logical classes of the toric code. It records their physical weights and generates histograms showing the weight distributions.

Run all commands from the root of the `rosette-QEC` repository.

## Step 1 — Run the Rosette Sampling Experiment

Generate 10,000 logical error samples for an $L=3$ toric code.

```bash
racket experiments/sample-logical-weights-all.rkt 3 10000
```

Output:

`results/toric-L3-all15-N10000.csv`

## Step 2 — Generate the Graphs Using Python

```bash
python3 python/plot-logical-weights-all.py results/toric-L3-all15-N10000.csv
```

This generates two graphs.

## Step 3 — Open the Overall Weight Histogram

```bash
open results/toric-L3-all15-N10000-histogram.png
```

Shows the frequency of physical Pauli weights across all 15 logical classes.

## Step 4 — Open the Distributions for All 15 Logical Classes

```bash
open results/toric-L3-all15-N10000-by-class.png
```

Shows the weight distribution for each logical class separately.

## Optional — Repeat for L=4

```bash
racket experiments/sample-logical-weights-all.rkt 4 10000
python3 python/plot-logical-weights-all.py results/toric-L4-all15-N10000.csv

open results/toric-L4-all15-N10000-histogram.png
open results/toric-L4-all15-N10000-by-class.png
```

## Output Summary

- **CSV:** Sample number, logical class, and physical Pauli weight.
- **Histogram:** Overall weight distribution.
- **By-class graph:** Weight distributions for each of the 15 nontrivial logical classes.