#!/usr/bin/env python3
"""Plot weight frequencies from the existing two-representative toric sampler."""

import argparse
import csv
import re
from collections import Counter
from pathlib import Path

import matplotlib.pyplot as plt


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("csv_file", type=Path, help="CSV written by sample-logical-weights.rkt")
    parser.add_argument("--L", type=int, default=None, help="Toric lattice size, if not present in filename")
    args = parser.parse_args()

    with args.csv_file.open(newline="") as f:
        reader = csv.DictReader(f)
        if not reader.fieldnames or "weight" not in reader.fieldnames:
            parser.error("the input CSV needs a 'weight' column")
        weights = [int(row["weight"]) for row in reader]

    if not weights:
        parser.error("the CSV has no samples")

    match = re.search(r"toric-L(\d+)", args.csv_file.name)
    L = args.L if args.L is not None else (int(match.group(1)) if match else None)
    counts = Counter(weights)
    possible_weights = range(min(weights), max(weights) + 1)

    fig, ax = plt.subplots(figsize=(9, 5))
    ax.bar(list(possible_weights), [counts[w] for w in possible_weights], width=0.85)
    ax.set_xlabel("Pauli weight")
    ax.set_ylabel("Frequency (number of samples)")
    name = f"L={L}" if L is not None else "toric code"
    ax.set_title(f"Toric code {name}: (N={len(weights)})")
    if L is not None:
        ax.axvline(L, color="gray", linestyle="--", alpha=0.8,
                   label=f"Code distance d={L}")
        ax.legend()
    if max(weights) - min(weights) <= 35:
        ax.set_xticks(list(possible_weights))
    fig.tight_layout()

    image_path = args.csv_file.with_name(args.csv_file.stem + "-histogram.png")
    fig.savefig(image_path, dpi=180)
    plt.close(fig)

    freq_path = args.csv_file.with_name(args.csv_file.stem + "-frequencies.csv")
    with freq_path.open("w", newline="") as f:
        writer = csv.writer(f)
        writer.writerow(["weight", "frequency"])
        writer.writerows((w, counts[w]) for w in possible_weights)

    print(f"Samples: {len(weights)}")
    print(f"Observed weight range: {min(weights)} to {max(weights)}")
    print(f"Histogram: {image_path}")
    print(f"Frequency table: {freq_path}")


if __name__ == "__main__":
    main()
