#!/bin/bash
#---------------Script SBATCH - NLHPC ----------------
#SBATCH -J enut-ii-pipeline
#SBATCH -p general
#SBATCH -n 1
#SBATCH --ntasks-per-node=1
#SBATCH -c 10
#SBATCH --mem-per-cpu=3000
#SBATCH --mail-user=pareyes2018@udec.cl
#SBATCH --mail-type=ALL
#SBATCH -t 1-0:0:0
#SBATCH -o enut-ii/pipeline_%A_%a.err.out
#SBATCH -e enut-ii/pipeline_%A_%a.err.out

# Full ENUT-II pipeline (steps.md, steps 2 to 4). The expenditure models
# (step 1, run_expenditures.bash) do not need to be rerun.
set -e
cd enut-ii

# ---------------- Step 2: pre weekend file ----------------
module load r/4.4.0
Rscript data_processing/data_processing.R --pre

# ---------------- Step 3: twin matrix ----------------
ml purge
ml intel/2022.00
ml Python/3.12.3
source enut-env/bin/activate
TWIN_WORKERS=${SLURM_CPUS_PER_TASK:-10} python data_processing/gemelos_matriz.py
deactivate

# ---------------- Step 4: rest of the pipeline ----------------
ml purge
module load r/4.4.0
Rscript data_processing/data_processing.R
