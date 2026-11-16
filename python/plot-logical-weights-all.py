
#!/usr/bin/env python3

"""Plot weight distributions for all 15 toric logical classes."""

import argparse
import csv
import re
from collections import Counter, defaultdict
from pathlib import Path

import matplotlib.pyplot as plt


def main():
    parser = argparse.ArgumentParser(
        description=__doc__
    )
    parser.add_argument("csv_file", type=Path)
    args = parser.parse_args()

    # Read samples and group their weights by logical class.
    by_class = defaultdict(list)

    with args.csv_file.open(newline="") as f:
        reader = csv.DictReader(f)

        if not {"class", "weight"}.issubset(
            reader.fieldnames or []
        ):
            parser.error(
                "CSV needs 'class' and 'weight' columns"
            )

        for row in reader:
            by_class[row["class"]].append(
                int(row["weight"])
            )

    if not by_class:
        parser.error("CSV has no samples")

    weights = [
        w
        for values in by_class.values()
        for w in values
    ]

    # Infer lattice size from the CSV filename.
    match = re.search(
        r"toric-L(\d+)",
        args.csv_file.name
    )
    L = int(match.group(1)) if match else None

    counts = Counter(weights)
    possible_weights = list(
        range(min(weights), max(weights) + 1)
    )

    ##########################################
    # Plot 1: Overall weight distribution    #
    ##########################################

    fig, ax = plt.subplots(figsize=(9, 5))

    ax.bar(
        possible_weights,
        [counts[w] for w in possible_weights],
        width=0.85
    )

    ax.set_xlabel("Physical Pauli weight")
    ax.set_ylabel("Number of samples")
    ax.set_title(
        f"Toric code L={L}: all 15 classes "
        f"(N={len(weights)})"
    )

    if L is not None:
        ax.axvline(
            L,
            color="gray",
            linestyle="--",
            label=f"Distance d={L}"
        )
        ax.legend()

    fig.tight_layout()

    overall_path = args.csv_file.with_name(
        args.csv_file.stem + "-histogram.png"
    )

    fig.savefig(overall_path, dpi=180)
    plt.close(fig)

    ##########################################
    # Plot 2: Distribution by logical class  #
    ##########################################

    fig, ax = plt.subplots(figsize=(10, 6))

    for label in sorted(by_class):
        values = by_class[label]
        hist = Counter(values)

        # Normalize by samples in this class.
        frequencies = [
            hist[w] / len(values)
            for w in possible_weights
        ]

        ax.plot(
            possible_weights,
            frequencies,
            marker=".",
            markersize=3,
            linewidth=1,
            label=label
        )

    ax.set_xlabel("Physical Pauli weight")
    ax.set_ylabel("Fraction of samples within class")
    ax.set_title(
        f"Toric code L={L}: weight distribution by class"
    )

    if L is not None:
        ax.axvline(
            L,
            color="gray",
            linestyle="--",
            label=f"Distance d={L}"
        )

    ax.legend(ncol=3, fontsize=8)
    fig.tight_layout()

    byclass_path = args.csv_file.with_name(
        args.csv_file.stem + "-by-class.png"
    )

    fig.savefig(byclass_path, dpi=180)
    plt.close(fig)

    # Print results.
    print(f"Samples: {len(weights)}")
    print(f"Classes observed: {len(by_class)} of 15")
    print(f"Weight range: {min(weights)} to {max(weights)}")
    print(f"Overall histogram: {overall_path}")
    print(f"By-class distributions: {byclass_path}")


if __name__ == "__main__":
    main()
