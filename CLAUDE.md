# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Purpose

Econometric research project studying time-use and expenditure patterns in Chile using ENUT 2023 (Encuesta Nacional de Uso de Tiempo) data. The core methodology is structural estimation of discrete choice models via the Apollo R package, implementing Jara-Diaz indirect utility functions to derive the value of time (VoL) and value of time to available work (VTAW).

---

## Full Pipeline

The pipeline has two offline prerequisite steps (run once when raw data changes) and a main R pipeline. Steps 0a and 0b are independent of each other and can run in parallel.

### Step 0a - Expenditure model (offline, one-time)

**Requires:** EPF IX raw data (not in this repository).

Fits a Fractional MNL model on household expenditure shares and a savings regression using EPF IX data. Produces `data/gastos.csv`, which is the lookup table used in Step 3 to impute expenditures into the ENUT dataset.

```r
source("data_processing/expenditures.R")
# Output: data/gastos.csv
```

Re-run only if EPF data or the expenditure model specification changes.

### Step 0b - Twin matrix (offline, one-time, Python)

**Requires:** Python with `cvxpy`, `polars`, `numpy`, `pandas`, `tqdm`.
**Requires:** `data/raw/ENUT_PRE_WEEKEND_IMPUTATION.csv` (produced by the first block of Step 1).

Computes a similarity-weight matrix used to impute weekend time-use diaries. For each respondent, solves a convex quadratic program that finds a convex combination of sociodemographically similar respondents from the complementary day type (weekday vs. weekend). The matching uses 18 social variables and a Mahalanobis distance metric.

```bash
# Run from the repository root
python data_processing/gemelos_matriz.py
# Outputs:
#   data/raw/matriz_gemelos2.npy   (rounded to 2 decimals)
#   data/raw/matriz_gemelos4.npy   (rounded to 4 decimals)
#   data/raw/matriz_gemelos5.npy   (rounded to 5 decimals, primary)
#   data/raw/matriz_gemelos5.csv.gzip
```

If the .npy file exists but the .csv.gzip does not (e.g., after cloning), convert it:

```bash
python data_processing/extract_twins.py
```

This step is computationally expensive (one QP per respondent, ~6000-8000 problems). Run on HPC or a machine with many cores; adjust `Pool(1, ...)` in `gemelos_matriz.py` to use more workers.

Re-run only if the pre-imputation dataset changes (i.e., if Step 1a output changes).

### Step 1 - Data processing pipeline (R)

**Requires:** Steps 0a and 0b complete.

```r
source("data_processing/data_processing.R")
```

Internally this runs in two sub-blocks:

**1a - Pre-imputation processing** (produces the input for Step 0b):
1. Load `data/raw/ii-enut-bdd-stata.dta`
2. `new_variables_prefilter()` - sociodemographic variables, household composition, income construction
3. `na_completion()` - missing time-use completion and activity re-aggregation
4. `new_variables_postfilter()` - derived variables (macrozona, edad_anios, horas_trabajo_contratadas, etc.)
5. `outlier_detection_Vallejo()` - removes individuals outside group-specific time-use bounds
6. Save `data/raw/ENUT_PRE_WEEKEND_IMPUTATION.dta/.csv`

**1b - Post-imputation processing** (requires twin matrix from Step 0b):
7. `impute_weekend()` - weighted average of matched twins fills in missing weekend diaries
8. `diagnostico_trabajo()` - filters observations where diary hours deviate too far from contracted hours
9. `adjust_working_hours()` - aligns paid work diary time with contracted schedule
10. `data_to168hours()` - rescales non-anchor activities so each row sums to 168 hours
11. `agregar_actividades()` - produces two versions:
    - 25-activity: `data/enut-ii-25.dta/.csv`
    - 11-activity: `data/enut-ii-11.dta/.csv`
12. `imputacion_gastos()` - merges `gastos.csv` expenditure model predictions:
    - `data/enut-ii-25G.dta/.csv`
    - `data/enut-ii-11G.dta/.csv`
13. `rename_to_english_25/11()` - English variable name versions:
    - `data/enut-ii-25G-ENG.dta/.csv`
    - `data/enut-ii-11G-ENG.dta/.csv`

### Step 2 - Exploration (optional)

Independent of modeling. Can be run any time after Step 1 completes.

```r
source("exploration.R")
# Outputs: plots/ directory
```

### Step 3 - Model estimation

```r
source("utils.R")
model_data <- get_data()       # 25-activity version (main)
model_data <- get_data_tc()    # 11-activity, two time-commitment definitions
model_data <- get_data_2tc()   # 11-activity, four time-commitment scenarios

source("models/quadratic.R")
source("models/thph1t1e.R")
# Outputs: output/*.rds, output/*.txt, output/*.csv
```

All model scripts source `utils.R` at the top. Do not call `get_data()` separately before sourcing a model script.

### Step 4 - Prediction

```r
source("prediction/prediction_thph1t1e.R")
source("prediction/prediction_4c_quadratic.R")
```

---

## Architecture

### Data Flow

```
data/raw/ii-enut-bdd-stata.dta
    │
    ├─[Step 1a]─→ data/raw/ENUT_PRE_WEEKEND_IMPUTATION.dta/.csv
    │                   │
    │             [Step 0b: Python]
    │                   │
    │             data/raw/matriz_gemelos5.csv.gzip
    │                   │
    ├─[Step 1b]─→ data/enut-ii-{11,25}.dta/.csv          (no expenditures)
    │                   │
    │             [Step 0a: expenditures.R → gastos.csv]
    │                   │
    ├─[Step 1b]─→ data/enut-ii-{11,25}G.dta/.csv         (with expenditures)
    │
    ├─[Step 1b]─→ data/enut-ii-{11,25}G-ENG.dta/.csv     (English names)
    │
    ├─[Step 2]──→ plots/
    │
    ├─[Step 3]──→ utils.R: get_data() → model_data
    │                   │
    │             models/*.R → output/
    │
    └─[Step 4]──→ prediction/*.R
```

### Key Files

- **`utils.R`** - Central hub: `get_data()`, `get_data_tc()`, `get_data_2tc()`, parameter initializers (`generate_initials_*`), and `report_values_of_time_thph()`. All model scripts source this.
- **`apollo_JaraDiaz.R`** - Custom Apollo likelihood implementing the Jara-Diaz structural model. Two variants: `apollo_jaradiaz()` and `apollo_jaradiaz_2pi()`.
- **`data_processing/processing_functions.R`** - All pipeline functions: `new_variables_prefilter()`, `na_completion()`, `new_variables_postfilter()`, `outlier_detection_Vallejo()`, `impute_weekend()`, `diagnostico_trabajo()`, `adjust_working_hours()`, `data_to168hours()`, `agregar_actividades()`, `imputacion_gastos()`, `rename_to_english_25()`, `rename_to_english_11()`.
- **`data_processing/gemelos_matriz.py`** - Twin matrix computation (Step 0b).
- **`data_processing/expenditures.R`** - Expenditure model producing `gastos.csv` (Step 0a).
- **`exploration.R`** - Descriptive statistics and plots (not part of the estimation pipeline).

### Model Structure

All models follow the same Apollo pattern:

1. Source `utils.R` and call `get_data()` to get `model_data`
2. Define `apollo_beta` (parameters), `apollo_fixed`, `apollo_control`
3. Define `apollo_probabilities()` using either built-in Apollo functions or `apollo_jaradiaz()`
4. Call `apollo_estimate()` with BGW optimizer (500 max iterations, 1000 max evals)
5. Save with `saveRDS()` to `output/`

Model naming convention: `thph` = TH (total hours) PH (paid hours) Jara-Diaz variant. Suffix `ntne` = multiple times/expenditures. `lc_` prefix = latent class. `_covariates` suffix = class allocation with socioeconomic covariates.

### Key Constants

- `ipc = 0.362` - CPI adjustment applied to expenditure data
- `z = 3` - Standard deviations threshold for outlier removal
- `nvals = 100` - Number of random starts per model (computationally expensive)
- `nClass` - Number of latent classes (2-4 in existing models)

### Data Filtering Criteria (applied in `get_data()`)

- Age >= 18
- `es_trabajador == 1` (employed workers only)
- Personal income > 0 and wage > 0
- Expenditure-to-income ratio <= 5

### Activity Classifications

Data exists in two temporal classifications: 11-activity (aggregated) and 25-activity (detailed). The `G` suffix datasets include imputed expenditures from `gastos.csv`. The variable `ec = Ec / (w * (ta - Tc))` is the normalized expenditure commitment ratio central to the Jara-Diaz model.

---

## Dependencies

### R

No `renv.lock` exists. Key packages: `apollo`, `haven`, `dplyr`, `tidyr`, `purrr`, `ggplot2`, `matrixcalc`, `comprehenr`, `reshape2`, `lubridate`, `chilemapas`. Install via `install.packages()` as needed.

### Python (Step 0b only)

```bash
pip install cvxpy polars numpy pandas tqdm
```

The `cvxpy` package requires a working ECOS or SCS solver. ECOS is the default; if it is not available, edit `gemelos_matriz.py` line 22 to use `solver=SCS`.

---

## HPC Execution

The twin matrix computation (Step 0b) and model estimation (Step 3) are the two computationally expensive steps.

- **Twin matrix:** parallelised via Python `multiprocessing.Pool`. Increase the worker count in `gemelos_matriz.py` line 73 (`Pool(1, ...)`) to match available cores.
- **Model estimation:** `nvals = 100` random-start loops in each model script are the main cost. HPC job logs are in `.err.out` files in the repository root.
