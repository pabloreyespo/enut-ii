# ENUT Pipeline Updates Summary

This document summarizes the changes applied to the ENUT-II data processing pipeline to assist in propagating these updates to the **ENUT-I** pipeline.

## 1. Dataset Nomenclature Changes
- **`enut-ii-25G`** has been renamed to **`enut-ii-raw`**.
- **`enut-ii-11G`** has been renamed to **`enut-ii`**.
- References to these files across scripts and documentation have been updated accordingly. 

## 2. Updated Time-Use Classification
The new `enut-ii` dataset now calculates **both** the original 11 aggregated classifications and a **new 10-category classification**.

**New Added Time Categories:**
- `Tw` (equivalent to `t_to` / `t_paid_work`)
- `Tf_social` (equivalent to `t_vsyo_csar`)
- `Tf_hobbies` (equivalent to `t_vsyo_aa`)
- `Tf_read` (equivalent to `t_mcm_leer`)
- `Tf_listen` (equivalent to `t_mcm_audio`)
- `Tf_watch` (equivalent to `t_mcm_video`)
- `Tf_computer` (equivalent to `t_mcm_computador`)
- `Tc_meals` (equivalent to `t_cpag_comer`)
- `Tc_sleep` (equivalent to `t_cpag_dormir`, mathematically constrained so activities sum to exactly 168 hours)
- `Tc_other` (equivalent to the sum of all other unmentioned activities)

*Note: In the code, `Tc_sleep` computes exactly as `t_sleep` by applying the residual difference from 168 hours to ensure perfect summation.*

## 3. Updated Expenditure Classification
The detailed granular expenditure classes have been strictly mapped to **6 broad categories** exclusively for the final `enut` dataset (while `enut-ii-raw` preserves the extended expenditure details natively imputed).

**New Expenditure Grouping:**
- `Ef_food` (`alimentos`)
- `Ef_recreation` (`recreacion`)
- `Ef_restaurants` (`restaurantes`)
- `Ef_communications` (`comunicaciones`)
- `Ef_clothing` (`vestimenta`)
- `Ec` (`cuentas` + `hogar` + `salud` + `transporte` + `educacion` + `savings`)

Unused residual columns (`alimentos`, `recreacion`, etc.) are dropped to keep `enut` streamlined.

## 4. Work Variable Inclusions
The variables representing **teleworking** and **work schedules** have been formalized natively into the dictionaries and exported datasets:
- `teletrabaja` (English: `teleworks`)
- `jornada_laboral` (English: `work_schedule`)

## 5. Script Structural Adjustments
1. **`processing_functions.R`**:
   - Updates `t_agregados` definitions and includes a new `t_agregados_new` for logic loops ensuring BOTH classifications are created natively.
   - Updates `agregar_actividades()` to build the new variables via `dplyr` manipulation.
   - Refactors translation functions (`rename_to_english_enut` / `_raw`) using `dplyr::any_of()` to robustly map English variables without crashing if they were previously replaced natively.
2. **`data_processing.R`**:
   - Applies aggregations for expenses explicitly inside the subset creation for `data_enut` immediately following `imputacion_gastos()`. 
   - Uses `write_csv` and `haven::write_dta` directly reflecting `-raw` and `-ENG` suffix permutations.
3. **`enut_ii_raw.R` & `enut_ii.R` (Roxygen Docs)**:
   - Contains accurately renamed filenames and extensively documents definitions and properties for the new variable schema to provide IDE tooltip hints properly.

## 6. Processing Fixes
Each fix was checked against the raw INE file by rebuilding the INE aggregates
from the item level times.

1. **Leisure items that were dropped.** INE `t_vsyo` is vs1 to vs6, but only
   `t_vsyo_csar` (vs1 + vs3) and `t_vsyo_aa` (vs4 + vs5) were kept. Added
   `t_vsyo_ev` (vs2, attending events), `t_vsyo_dep` (vs6, sports and
   exercise) and `t_descanso` (vs11, rest). New classification variables:
   `Tf_events`, `Tf_sports`, `Tf_rest`; `t_leisure` now includes events and
   sports and `t_rest` is new.
2. **Pension sign in household income.** `ing_g` added `ing_jub_aps` instead
   of subtracting it, overstating `ingreso_hogar` by twice the pensions (38% of
   households). With the fix other income is never negative.
3. **168 hour closure of `enut-ii`.** `t_agregados` omitted commuting while
   `Tc_other` included it, so the Tw/Tf/Tc classification summed to 171.7 hours
   on average. `t_commute` and `t_job_search` are now in `t_agregados`, each
   classification gets its own sleep residual, and `agregar_actividades()`
   stops if any classification misses 168 hours.
4. **Education commute counted twice.** INE `t_ed` includes ed2 and ed5, which
   were also added as `t_ted`. They are now removed from `t_ed`.
5. **Care commute counted twice.** INE `t_tcnr_oac` includes tc31 and tc34
   (as well as tc21 and tc25), which were also added as `t_ttcnr_oac_work`.
6. **Paid work included commuting and job search.** INE `t_to` is
   to3 + to5 + to7 + to9. The diary value now excludes the commutes (already
   in `t_tto`) and job search, which is kept as `t_to_js`. This affects the
   Vallejo filter, the weekday rescaling and the diary versus contract check;
   `t_to` is still replaced by contracted hours afterwards.
7. **Expenditure imputation coding.** `n_menores_5_14_cut` used `>= 3 ~ 3`
   while the EPF data the FMNL and savings models were fitted on
   (`expenditures.R`) uses `>= 2 ~ 3`. The ENUT side now uses the EPF coding.
8. **Minor.** `hay_tercera_edad` operator precedence; the IPC 0.362 deflation
   claimed in the docs is not applied anywhere (money is nominal 2023 CLP);
   `t_cpaf_cp` does not contain exercise; `outlier_detection_Vallejo()` ended
   in `%>% return(data)` (it worked, now a plain return).
9. **Shared structure with enut-i.** `t_agregados` and `t_agregados_new` have
   the same names and definitions in both pipelines (see enut-i `CHANGES.md`
   for the item mapping). `t_commute` replaces `t_commute1`/`t_commute2`.
8. **Twin matrix script.** The Mahalanobis term is computed with numpy instead
   of `diag(quad_form(...))`, which built a dense (n - 1) x (n - 1) matrix per
   individual; the result is the same. Workers return their row instead of
   receiving a copy of the full matrix, the worker count comes from
   `TWIN_WORKERS` (default: all cores) and the output is streamed.

Because fixes 1, 2, 4 and 6 change the Vallejo filter and the quintiles, the
pre weekend sample changes (15,008 to 15,199 rows) and the twin matrix must
be rebuilt before running the rest of the pipeline.

## 7. Professionals in the expenditure models

`n_profesionales` in `expenditures.R` counted `edunivel >= 14`. That cut comes
from the VIII EPF coding, where 14 is a professional degree; in the IX EPF 14 is
only a doctorate (89 people in the whole survey), so almost no EPF household had
professionals while most ENUT households do (`nivel_escolaridad >= 4`). The FMNL
and savings coefficients on `n_profesionales_cut` were therefore estimated on a
handful of households and extrapolated to the ENUT. It now counts a completed
technical tertiary, professional or postgraduate degree (`edunivel >= 10` and
`edutermina != 2`, 9,827 people), the same group the ENUT counts.

Because the EPF covariate changes, `data/gastos.csv` and the `FMNL-epf-ix` model
must be re-estimated (`run_expenditures.bash`) before running
`data_processing.R` again.

## 8. Expenditure imputation with EPF strata (Hawkins hot-deck)

`imputacion_gastos()` now has `metodo = "hawkins"` (default) besides the FMNL
(`metodo = "fmnl"`). The household savings regression is unchanged; the
composition of expenditure (the 10 divisions) comes from a cell hot-deck in the
spirit of Hawkins (2021, ch. 7): each ENUT household gets the mean of the
expenditure shares of m EPF households drawn from its cell, with m = household
members aged 15 or more. Cells, finest first (the first with at least 10 EPF
households is used):

1. EPF sampling stratum x quintile. The stratum follows from the comuna and the
   NSE of the ENUT household: the comuna is now kept as `cut` (from `varunit`) and
   `data/epf_estratos_comunas.csv` lists the comunas and socioeconomic level of
   each of the 79 IX EPF strata (Metodologia IX EPF, Tabla 16).
2. Region x NSE x quintile (comunas that are not in the EPF).
3. Macrozone x NSE x quintile. 4. Macrozone x quintile.

Assumption: the ENUT `NSE` is the same UPM classification used by the EPF
(1 low, 2 middle, 3 high). On the current `enut-ii-raw` sample, 74% of households
use cell 1, 24% cell 2 and 2% cell 3.

Cross-validation on the EPF (5 folds, Ef1/Ef2/Ec shares): the hot-deck has a
larger error per household than the FMNL (RMSE of the Ef1 share 0.197 vs 0.160)
but keeps 78% of the observed dispersion instead of 32%, and keeps households
with zero spending in a division. Using the strata lowers the hot-deck error by
3% relative to the original Hawkins cells (macrozone x quintile x household
size).

Other changes:
- `expenditures.R` keeps the stratum (`estrato`) and household NSE (`nse`) in
  `data/gastos.csv`, and `Rscript data_processing/expenditures.R --solo-datos`
  writes that file without re-estimating the FMNL (the hot-deck only needs it).
- New household variable `n_15_mas` (members aged 15+).
- `n_profesionales` in the ENUT uses `na.rm = TRUE`: a member without education
  level left the whole household with `NA` and no imputed expenditure (7 people).
