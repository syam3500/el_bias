#!/bin/bash -l
#SBATCH --account=lp_h_nn_thesis
#SBATCH --cluster=wice
#SBATCH --nodes=1
#SBATCH --cpus-per-task=8              # Increased: one worker per .qmd in parallel
#SBATCH --mem=32G                      # Scaled with parallelism (4 GB per worker)
#SBATCH --time=10:00:00

# --------------------------------------------------------------------------- #
# Environment setup
# --------------------------------------------------------------------------- #
module load R/4.4.1-gfbf-2023b
module load quarto/1.5.57
module load Python/3.12

if [ -f ~/ml_env/bin/activate ]; then
    source ~/ml_env/bin/activate
else
    echo "Warning: ml_env not found, using system Python"
fi
export RETICULATE_PYTHON=$(which python)
echo "Using Python at: $RETICULATE_PYTHON"

# Limit R/OpenBLAS thread count so parallel Quarto jobs do not fight over cores
export OMP_NUM_THREADS=1
export OPENBLAS_NUM_THREADS=1
export MKL_NUM_THREADS=1

# Install only missing packages — avoids redundant pip overhead on every run
python - <<'PYEOF'
import importlib.util, subprocess, sys
pkgs = ["joblib", "scikit-learn", "pandas", "numpy"]
missing = [p for p in pkgs if importlib.util.find_spec(p) is None]
if missing:
    print(f"Installing missing packages: {missing}")
    subprocess.check_call(
        [sys.executable, "-m", "pip", "install", "--quiet"] + missing
    )
else:
    print("All Python packages already present — skipping pip install.")
PYEOF

# --------------------------------------------------------------------------- #
# Parallel Quarto rendering
# --------------------------------------------------------------------------- #
N_JOBS=${SLURM_CPUS_PER_TASK:-4}   # use however many CPUs SLURM granted

# Resolve absolute paths upfront so parallel subshells are not affected by cwd
mapfile -t QMD_FILES < <(ls *.qmd 2>/dev/null | xargs -I{} realpath {})

if [ ${#QMD_FILES[@]} -eq 0 ]; then
    echo "No .qmd files found. Exiting."
    exit 0
fi

echo "Found ${#QMD_FILES[@]} file(s). Rendering with up to ${N_JOBS} parallel workers..."

render_file() {
    local abs_file="$1"
    local base
    base=$(basename "$abs_file")
    local dir
    dir=$(dirname "$abs_file")
    echo "→ Starting : $base"
    if (cd "$dir" && quarto render "$base" --to gfm -M fig-format:png); then
        echo "✓ Done     : $base"
    else
        echo "✗ FAILED   : $base" >&2
        return 1
    fi
}
export -f render_file

# Prefer GNU parallel (available on most HPC clusters); fall back to bash jobs
if command -v parallel &>/dev/null; then
    printf '%s\n' "${QMD_FILES[@]}" \
        | parallel --jobs "${N_JOBS}" --joblog parallel_render.log render_file {}
else
    # Pure-bash semaphore: keep at most N_JOBS children alive at once
    active=0
    for file in "${QMD_FILES[@]}"; do
        render_file "$file" &
        (( active++ ))
        if (( active >= N_JOBS )); then
            wait -n 2>/dev/null || wait    # wait for any one child to finish
            (( active-- ))
        fi
    done
    wait    # drain remaining background jobs
fi

echo "All renders complete."
