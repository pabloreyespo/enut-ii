# Execution Steps for Processing the ENUT-II Pipeline

Below is the complete step-by-step workflow required to process the data from absolute zero.

### 1. Run the Expenditures Model Definition
**Script to run:** `data_processing/expenditures.R`
- **What it does**: This script loads the EPF surveys (`base-personas-ix-epf-stata.dta` and `base-gastos-ix-epf-stata.dta`). It maps the variables, creates the standard `data/gastos.csv` file, and estimates the Fractional MNL model via Apollo (`FMNL-epf-ix`).
- **Why do it now**: The models and the `.csv` generated here are strict dependencies required later when ENUT time records try to impute their budget shares. 

### 2. Generate the Preliminary ENUT DataFrame
**Script to run:** `data_processing/data_processing.R` (Only up to Line 64)
- **What it does**: Execute the script selectively from the top down to `write_csv(..., "data/raw/ENUT_PRE_WEEKEND_IMPUTATION.csv")`. 
- **Why do it now**: This reads the raw ENUT-II dataset, generates the basic mapping and prefilters, formats variables, runs Vallejo's outlier detection, and spits out the preliminary dataset. The resulting CSV acts as the direct, clean input required by Python in the next step to figure out covariance profiles.
- *(Note: Do NOT run past line 64 yet, because R will crash trying to search for the twin matrix we haven't built yet).*

### 3. Generate the Twin Matrix
**Script to run:** `data_processing/gemelos_matriz.py` (or through the interactive `.ipynb`)
- **What it does**: This loads the freshly minted `ENUT_PRE_WEEKEND_IMPUTATION.csv` file into Python. Using multiprocessing and convex optimization (`cvxpy` + `ECOS` solvers), it finds comparable individuals and calculates the optimal weights for twin matching based heavily on Mahalanobis/Covariance distances.
- **Why do it now**: Because it mathematically calculates the missing proxy variables required for imputing weekend time allocations.
- *(Important Note: Make sure the output artifact name matches what R expects. The Python script currently outputs `matriz_gemelos5.csv.gzip`, but `data_processing.R` expects `"data/raw/matriz_gemelos.csv.gzip"`. Rename appropriately in the file system, or edit the `read_csv` line in `data_processing.R` space before running step 4).*

### 4. Complete the Pipeline
**Script to run:** `data_processing/data_processing.R` (From Line 66 onwards)
- **What it does**: Resume execution from `twin_matrix <- read_csv(...)` down to the end of the script.
- **Why do it now**: Armed with the twin matrix file and the pre-computed expenditure models, this script seamlessly imputes missing weekend diary data (`impute_weekend`), merges activities into their 168-hour groupings (`agregar_actividades` which captures the new `Tw` classifications), pulls expenditure budget shares utilizing the Apollo FMNL models (`imputacion_gastos()`), maps translations (`rename_to_english`), and finally automatically spits out your completed `enut-ii-raw` and `enut-ii` datasets.
