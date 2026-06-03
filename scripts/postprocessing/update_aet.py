"""
update_aet.py
-------------
Batch-updates the AET value in every .qmd file in the target directory.

Usage
-----
python update_aet.py --aet 0.45
python update_aet.py --aet 6 --dir ../../scripts/simulation

Arguments
---------
--aet   float   New AET value to set (replaces the current AET <- <value> line).
--dir   path    Directory containing .qmd files to update.
                Defaults to the scripts/simulation folder relative to this script.
"""

import os
import re
import argparse


def update_aet(directory: str, new_aet: float) -> None:
    pattern = re.compile(r'^AET\s*<-\s*[\d.]+', re.MULTILINE)
    replacement = f"AET <- {new_aet}"
    updated = []

    for root, _, files in os.walk(directory):
        for fname in files:
            if not fname.endswith('.qmd'):
                continue
            filepath = os.path.join(root, fname)
            try:
                with open(filepath, 'r', encoding='utf-8') as f:
                    content = f.read()
                new_content = pattern.sub(replacement, content)
                if new_content != content:
                    with open(filepath, 'w', encoding='utf-8') as f:
                        f.write(new_content)
                    updated.append(fname)
            except Exception as e:
                print(f"Error processing {filepath}: {e}")

    print(f"AET set to {new_aet} in {len(updated)} file(s).")
    for name in sorted(updated):
        print(f"  {name}")


if __name__ == "__main__":
    here = os.path.dirname(os.path.abspath(__file__))
    default_dir = os.path.normpath(os.path.join(here, '..', 'simulation'))

    parser = argparse.ArgumentParser(description="Batch-update AET value in simulation .qmd files.")
    parser.add_argument('--aet',  type=float, required=True,
                        help='New AET value in µg/mL (e.g. 0.45, 3, 6, 36)')
    parser.add_argument('--dir',  type=str,   default=default_dir,
                        help=f'Directory with .qmd files (default: {default_dir})')
    args = parser.parse_args()

    if not os.path.isdir(args.dir):
        raise FileNotFoundError(f"Directory not found: {args.dir}")

    update_aet(args.dir, args.aet)
