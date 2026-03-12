#!/bin/bash -l
#SBATCH --account=lp_h_ds_students   # Your specific account 
#SBATCH --cluster=wice              # The cluster name 
#SBATCH --cpus-per-task=4             # Adjust based on parallel needs 
#SBATCH --mem=16G                     # RAM request 
#SBATCH --time=06:00:00               # Simulations often take longer 

module load R/4.4.1-gfbf-2023b
module load quarto/1.5.57
module load Python/3.12

source ~/ml_env/bin/activate
export RETICULATE_PYTHON=$(which python)
echo "Using Python at: $RETICULATE_PYTHON"

pip install joblib scikit-learn pandas numpy

for file in *.qmd; do
    [ -e "$file" ] || continue
    echo "Processing $file..."
    quarto render "$file" --to gfm -M fig-format:png
done

echo "All renders complete."