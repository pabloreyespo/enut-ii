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

## 9. Savings from the same donors (`_jdi`) and FMNL aggregates (`_fmnl`)

`imputacion_gastos()` now always writes two sets of broad categories (Ef1 =
food + recreation + restaurants + communications, Ef2 = clothing, Ec = bills +
household + health + transport + education), allocated to each person by
`prop_ing_hogar` like the other amounts:

- `Ef1_jdi`, `Ef2_jdi`, `Ec_jdi`, `savings_jdi`: cell hot-deck where savings
  and expenditure come from the same EPF donors. Each donor contributes its
  (10 divisions, savings) / household income vector, which sums to one; the
  household gets the mean over its donors times its own income, so
  Ef1 + Ef2 + Ec + savings = income. The draw and the cells are the ones of
  section 8 (the same random draw as the unsuffixed hot-deck columns). The
  scheme is inspired by the data fusion of Hawkins and Habib (2022,
  Transportation), which averages the household expenditure of one record per
  member aged 15 or more.
- `Ef1_fmnl`, `Ef2_fmnl`, `Ec_fmnl`, `savings_fmnl`: FMNL shares times
  (income - savings), with savings from the regression.

Why: the savings regression has no residual, so it removed the dispersion of
savings (R2 between 0.36 and 0.59 in the EPF of the three big cities), and,
being linear in pesos, it predicted savings rates below -1 for low incomes,
outside the EPF support (the EPF sample drops those households). Taking savings
from the donors keeps its dispersion, its relation with the composition of
expenditure and the EPF support. The same change was made in enut-bigcities.

The unsuffixed columns (the 10 divisions, `savings`, `total_expenses`) are
unchanged, so existing models give the same results. The FMNL model
(`output/FMNL-epf-ix`) is now always loaded.

## 10. Joint distribution inference for the expenditure draw (`metodo = "jdi"`)

`imputacion_gastos()` now defaults to `metodo = "jdi"`; the cell hot-deck of
section 8 stays available as `metodo = "hotdeck"` (and the FMNL as `"fmnl"`).
Output columns and units are unchanged (`Ef1/Ef2/Ec/savings_jdi` still add up to
the household income, the unsuffixed divisions are the donors' mean shares times
income minus savings), so downstream code keeps working.

Method (Ye et al. 2017, JASSS, sections 3.1-3.13; Hawkins and Habib 2022,
Transportation, "Data fusion by joint distribution inference"). ENUT (time use)
and EPF (expenditure) share only household covariates D, so there is no table
that crosses time use with expenditure and f(D, time, exp) = f_ENUT(D, time) *
g_EPF(exp | D). Imputing means drawing EPF households from g(exp | D):

1. Dimensions D: income quintile, `region`, household members aged 15+
   (`n_15_mas`, binned 1, 2, 3, 4+) and workers (`n_trabajadores`, binned 0, 1,
   2, 3+). Dwelling type is not in the ENUT and is dropped.
2. Independence tests (`pruebas_independencia()`): weighted chi-square (weights
   normalised to the EPF sample size) of each dimension against quintiles of
   savings/income, of the Ec share and of the Ef1 share, alpha = 0.05. A
   dimension independent of the three would leave D (Ye eq. 9); the quintile
   never leaves. The strength is the largest Cramer's V. Result on the current
   data: all four dimensions are associated with the three classes (p < 1e-7);
   V is 0.13/0.08/0.09 for the quintile, 0.06-0.08 for region, 0.04-0.09 for
   members 15+ and 0.06 for workers, so workers is the weakest (dropped
   first), then region. Printed on every run and saved to `output/jdi_chi2.csv`.
3. IPF (`ipf_multivariado()`): EPF expansion factors (`fe`) are raked so that
   the one-way and two-way margins of the retained dimensions match the ENUT
   household distribution (weighted by `fe_ch`, the household expansion factor of
   the ENUT; `data_processing.R` passes it as `pesos`). Tolerance 1e-8, at most
   1000 sweeps, error if it does not converge. Current data: 18 sweeps, error
   9.6e-09, no ENUT mass outside the EPF support.
4. Draws by member (`sorteo_jdi()`): each ENUT household draws
   m = max(1, `n_15_mas`) EPF households with replacement, with probability
   proportional to the raked weight among those with its exact profile. With
   fewer than 10 donors the weakest dimension is dropped, then the next, never
   the quintile. The level used per household is returned as `nivel` (and saved
   to `output/jdi_nivel.csv`).
5. As before, the mean of the donors' (10 divisions, savings) / household income
   is multiplied by the ENUT household income. Seed 2023.

On the current ENUT households (10,976): 66% use the full profile
(quintile + region + members 15+ + workers), 27% drop workers and 8% drop workers
and region. In the hot-deck 54% used stratum x quintile.

`expenditures.R` now keeps `fe` and `n_15_mas` in `data/gastos.csv`
(`n_trabajadores` already counted dependent plus independent labor income).
Tests: `Rscript tests/test_jdi.R`.

Differences to the spec: the ENUT expansion factor is household-level `fe_ch`;
region is the EPF stratum region (`gastos.region`) against `cut %/% 1000` in the
ENUT; the savings regression and FMNL are untouched.

References: Ye, P., Hu, X., Yuan, Y., Wang, F.-Y. (2017).
Population synthesis based on joint distribution inference without disaggregate
samples. JASSS. Hawkins, J., Habib, K. N. (2022). A multisource data fusion
framework for joint population, expenditure, and time use synthesis. Transportation, doi:10.1007/s11116-022-10279-8.

## Household income split (`repartir_ingreso()`)
- Rents (`y7b`) and transfers from outside the household such as child support (`y8b`) are now the
  income of whoever receives them (`ing_otros`). Before, they fell into the household residual `ing_g`,
  which was split in proportion to labor and pension income, so a person without a salary who
  received child support ended up with zero income, expenditure and savings.
- If nobody in the household has own income, the residual is split equally among members aged 15+.
  Before, `0/0` was replaced by 0 and the whole household income disappeared (252 households).
- `ing_gpp` = `ing_otros` + share of the residual, so `ing_personal = ing_trab + ing_jub_aps + ing_gpp`
  and `fixed_income` in `utils.R` are unchanged. `I` now includes `ing_otros`. `ingreso_hogar` and the
  quintiles do not change.
- Tests: `Rscript tests/test_ingresos.R`.

## 11. Patch of the published datasets (`data_processing/parche_gastos_jdi.R`)

The full pipeline cannot be rerun as is (it needs a twin matrix that no longer matches the current
specification, see steps.md), so the JDI draw of section 10 is applied directly to the published
datasets: `Rscript data_processing/parche_gastos_jdi.R`. The script reads `data/enut-ii.dta` and
`data/enut-ii-raw.dta`, rebuilds the household table (`quintil`, `n_trabajadores`, `ingreso_hogar`,
`n_15_mas`, `region = cut %/% 1000`, and the household expansion factor `fe_ch` from
`ii-enut-bdd-stata.dta`), calls `imputar_jdi()` with seed 2023 and recomputes only `Ef1_jdi`,
`Ef2_jdi`, `Ec_jdi` and `savings_jdi` (ratios x `ingreso_hogar` x `prop_ing_hogar`, rounded to 2
decimals). All other columns and the 13,373 rows are unchanged (checked in the script, including the
`-ENG` renames). It rewrites the eight variants (raw, aggregated, `-ENG`; csv and dta) and
`output/jdi_chi2.csv` and `output/jdi_nivel.csv`.

Result on the 9,947 published households: IPF in 18 sweeps (error 8.9e-09, no mass outside the EPF
support); 6,580 households use the full profile, 2,595 drop workers and 772 drop workers and region.
At person level the `_jdi` means go from 47.87, 3.31, 51.85, 12.79 (Ef1, Ef2, Ec, savings) to 48.92,
3.48, 53.60, 9.83, and the SD from 72.3, 6.5, 85.7, 74.4 to 74.8, 7.2, 92.9, 67.1 (correlation old/new
0.77, 0.50, 0.79, 0.26).

## 12. Wage-rate outlier filter (`filtrar_salarios_extremos()`)

`data_processing.R` now drops extreme wage rates right after the time-use filters (Vallejo filter,
weekend imputation, `diagnostico_trabajo`, `adjust_working_hours`, `data_to168hours`) and before
`imputacion_gastos()`. The wage rate is `w = ing_trab / t_to`, the same definition used later for
the published `w`, with `t_to` already adjusted to contracted hours at that point.

* Among workers with a positive, finite `w`, rows whose `log(w)` lies outside
  `[Q1 - k IQR, Q3 + k IQR]` are dropped. `k` is the named argument `k = 3`.
* Rows without a positive `w` (non-workers) are kept untouched.
* Rows before and after are printed, as `diagnostico_trabajo()` does.
* Same rule as `drop_extreme_wages()` in the `mtuem-lc-activity-participation-py` project.
* Tests: `Rscript tests/test_salarios.R`.
* Indicative effect on the current published `enut-ii.csv` (the real filter runs earlier, so the exact
  counts will differ): 34 of 6105 workers dropped (22 low, 12 high); `w` range goes from 0.057 to 489.9 to 0.19 to 73.8.
* Rerun the full pipeline (`sbatch run_pipeline.bash`) to apply it.
