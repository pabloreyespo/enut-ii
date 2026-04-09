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
