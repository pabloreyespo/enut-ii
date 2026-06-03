# Commute Activity Times: Methodology

## Overview

This document describes how commute time is decomposed into 8 separate totals and how activity time variables (`tact_*`) are computed within the ENUT-II dataset processing pipeline.

## 1. Pipeline Overview

```
Raw Stata data (ii-enut-bdd-stata.dta)
    |
    v
new_variables_prefilter()      # Initial variable creation
    |
    v
na_completion()                # Compute commute totals from raw components
    |                           # Store _ds/_fds variants
    v
new_variables_postfilter()     # Post-processing
    |
    v
outlier_detection_Vallejo()    # Remove outliers by quintil/trabaja/tramo_edad/sexo
    |                           # Filter to: acts_corregidas, acts_care_alt_base,
    |                           #           tact_component_vars (_ds/_fds)
    v
impute_weekend()               # Twin matrix imputation for weekend days
    |                           # Creates _sab/_dom columns for all variables
    |                           # Scales to 24 hours per day
    |                           # Redistributes weekdays to fit 24*5 - t_to budget
    |                           # Stores normalization factors
    v
diagnostico_trabajo()          # Filter by t_to vs contracted hours difference
    |
    v
adjust_working_hours()         # Set t_to = horas_trabajo_contratadas
    |
    v
data_to168hours()              # Scale non-confiable activities to sum to 168 hours
    |
    v
agregar_actividades()          # Compute tact_* activity times
    |                           # Create data25 (25 activities) and data11 (11 aggregates)
    v
imputacion_gastos()            # Expenditure imputation from EPF IX
    |
    v
Output: enut-ii-raw.dta, enut-ii-raw.csv, enut-ii-raw-ENG.dta, etc.
```

## 2. Commute Classification

### 2.1 Eight Commute Totals

Commute time is classified by the macro-activity it is associated with. The classification is defined in `clasificacion_traslado.csv` and implemented in `processing_functions.R`.

| Variable | Macro-activity | Components (ida + vuelta) | Raw variables |
|----------|---------------|---------------------------|---------------|
| `t_tto` | Paid work (`t_to`) | to3_t + to7_t | Work commute |
| `t_ted` | Education (`t_ed`) | ed2_t + ed5_t | Education commute |
| `t_tcpaf_cp` | Personal care / health (`t_cpaf_cp`) | cp7_t + cp10_t | Health/personal care commute |
| `t_ttdnr_admnhog` | Domestic admin (`t_tdnr_admnhog`) | td14_t + td17_t | Domestic administration commute |
| `t_ttdnr_comphog` | Domestic shopping (`t_tdnr_comphog`) | td20_t + td23_t | Domestic shopping commute |
| `t_ttcnr_re` | Care - education (`t_tcnr_re`) | tc12_t + tc15_t | Care education commute |
| `t_ttcnr_oac_health` | Care - other (`t_tcnr_oac`) | tc21_t + tc25_t | Care essential commute |
| `t_ttcnr_oac_work` | Care - other (`t_tcnr_oac`) | tc31_t + tc34_t | Care other commute |

### 2.2 Commute Total Computation (na_completion)

In `na_completion()`, commute totals are computed by summing their inbound and outbound components:

```r
t_tto_ds = to3_t_ds + to7_t_ds
t_tto_fds = to3_t_fds + to7_t_fds
```

The macro-activity totals are adjusted by subtracting commute components:

```r
t_cpaf_cp_ds = t_cpaf_cp_ds - cp7_t_ds - cp10_t_ds
t_tdnr_admnhog_ds = t_tdnr_admnhog_ds - td14_t_ds - td17_t_ds
t_tdnr_comphog_ds = t_tdnr_comphog_ds - td20_t_ds - td23_t_ds
t_tcnr_re_ds = t_tcnr_re_ds - tc12_t_ds - tc15_t_ds
t_tcnr_oac_ds = t_tcnr_oac_ds - tc21_t_ds - tc25_t_ds
```

**Note:** `t_tcnr_ce` retains tc21_t and tc25_t (not subtracted).

## 3. Component Variables (Numerators)

Each activity time variable has a specific numerator representing the associated activity:

| Activity Time | Component | Description | Part of |
|--------------|-----------|-------------|---------|
| `tact_tto` | `to5_t` | Workplace time | `t_to` (NOT redistributed) |
| `tact_ted` | `ed4_t` | Education center time | `t_ed` (redistributed) |
| `tact_tcpaf_cp` | `cp9_t` | Health/personal care center time | `t_cpaf_cp` (redistributed) |
| `tact_ttdnr_admnhog` | `td16_t` | Domestic admin time | `t_tdnr_admnhog` (redistributed) |
| `tact_ttdnr_comphog` | `td22_t` | Domestic shopping time | `t_tdnr_comphog` (redistributed) |
| `tact_ttcnr_re` | `tc16_t` | Care education time | `t_tcnr_re` (redistributed) |
| `tact_ttcnr_oac_health` | `tc22_t` | Care health time | `t_tcnr_oac` (redistributed) |
| `tact_ttcnr_oac_work` | NA | No single associated activity | Always NA |

## 4. Twin Imputation for Component Variables

### 4.1 Problem

After twin imputation (`impute_weekend()`), the commute totals (`t_tto`, etc.) have `_sab` and `_dom` columns with imputed weekend values. However, the component variables (`to5_t`, `ed4_t`, etc.) originally only had `_ds` and `_fds` columns.

### 4.2 Solution

The `impute_weekend()` function was extended to also impute the component variables. This ensures both the component and its parent activity use the same imputed values.

### 4.3 Imputation Steps

**Step 1: Twin matrix imputation for Saturday (i=6)**

For weekend respondents (dia_fin_semana == 6):
```r
data[mask, tact_finde] = data[mask, tact_fds]
```

For weekday respondents:
```r
data[!mask, tact_finde] = twin_matrix[!mask, mask] %*% data[mask, tact_fds]
data[!mask, tact_finde] = data[!mask, tact_finde] / rowSums(twin_matrix[!mask, mask])
```

Same process for Sunday (i=7).

**Step 2: Weekend scaling to 24 hours**

```r
tact_sab = tact_sab * (24 / sum_sabados)
tact_dom = tact_dom * (24 / sum_domingos)
```

Where `sum_sabados` = sum of all activities on Saturday for that respondent.

**Step 3: Weekday scaling by work days**

```r
tact_ds = ifelse(dias_trabajo_semana < 5 & dias_trabajo_semana > 0,
                 tact_ds * dias_trabajo_semana,
                 tact_ds * 5)
```

**Step 4: Weekday redistribution for non-work components**

```r
weekday_factor = (24*5 - t_to_ds) / sum(non_t_to activities)
tact_ds_nonwork = tact_ds_nonwork * weekday_factor
```

**Note:** `to5_t` (workplace time) is part of `t_to` and is NOT redistributed.

**Step 5: Full week computation**

```r
tact_semana_completa = tact_ds + tact_sab + tact_dom
```

**Step 6: Store normalization factors**

```r
tact_factor_sab = 24 / sum_sabados
tact_factor_dom = 24 / sum_domingos
tact_factor_ds = weekday_factor
```

## 5. Activity Time Computation (agregar_actividades)

After twin imputation, activity times are computed in `agregar_actividades()`:

```r
# Weekday values (already normalized)
tact_tto_ds = to5_t_ds
tact_ted_ds = ed4_t_ds
tact_tcpaf_cp_ds = cp9_t_ds
tact_ttdnr_admnhog_ds = td16_t_ds
tact_ttdnr_comphog_ds = td22_t_ds
tact_ttcnr_re_ds = tc16_t_ds
tact_ttcnr_oac_health_ds = tc22_t_ds
tact_ttcnr_oac_work_ds = NA

# Weekend values (sab + dom, already normalized)
tact_tto_fds = to5_t_sab + to5_t_dom
tact_ted_fds = ed4_t_sab + ed4_t_dom
tact_tcpaf_cp_fds = cp9_t_sab + cp9_t_dom
tact_ttdnr_admnhog_fds = td16_t_sab + td16_t_dom
tact_ttdnr_comphog_fds = td22_t_sab + td22_t_dom
tact_ttcnr_re_fds = tc16_t_sab + tc16_t_dom
tact_ttcnr_oac_health_fds = tc22_t_sab + tc22_t_dom
tact_ttcnr_oac_work_fds = NA
```

## 6. Normalization Factors

Three normalization factors are saved per respondent:

| Factor | Formula | Description |
|--------|---------|-------------|
| `tact_factor_sab` | `24 / sum(all activities on Saturday)` | Scales Saturday activities to sum to 24 hours |
| `tact_factor_dom` | `24 / sum(all activities on Sunday)` | Scales Sunday activities to sum to 24 hours |
| `tact_factor_ds` | `(24*5 - t_to_ds) / sum(non-t_to activities on weekdays)` | Scales non-t_to weekday activities to fit 24*5 - t_to budget |

These factors are applied uniformly to all activities on a given day type. They allow reconstruction of the original unnormalized times if needed.

## 7. Variable Summary

### 7.1 Input Variables (Raw from ENUT-II)

| Variable | Description |
|----------|-------------|
| `to5_t` | Workplace time (per day) |
| `ed4_t` | Education center time (per day) |
| `cp9_t` | Health/personal care center time (per day) |
| `td16_t` | Domestic admin time (per day) |
| `td22_t` | Domestic shopping time (per day) |
| `tc16_t` | Care education time (per day) |
| `tc22_t` | Care health time (per day) |

### 7.2 Intermediate Variables (Computed in na_completion)

| Variable | Description |
|----------|-------------|
| `t_tto` | Commute to work total |
| `t_ted` | Commute to education total |
| `t_tcpaf_cp` | Commute to health/personal care total |
| `t_ttdnr_admnhog` | Commute to domestic admin total |
| `t_ttdnr_comphog` | Commute to domestic shopping total |
| `t_ttcnr_re` | Commute to care-education total |
| `t_ttcnr_oac_health` | Commute to care-essential total |
| `t_ttcnr_oac_work` | Commute to care-other total |

### 7.3 Output Variables (Computed in agregar_actividades)

| Variable | Unit | Description |
|----------|------|-------------|
| `tact_tto_ds` | hours | Workplace time, weekday |
| `tact_tto_fds` | hours | Workplace time, weekend |
| `tact_ted_ds` | hours | Education center time, weekday |
| `tact_ted_fds` | hours | Education center time, weekend |
| `tact_tcpaf_cp_ds` | hours | Health/personal care center time, weekday |
| `tact_tcpaf_cp_fds` | hours | Health/personal care center time, weekend |
| `tact_ttdnr_admnhog_ds` | hours | Domestic admin time, weekday |
| `tact_ttdnr_admnhog_fds` | hours | Domestic admin time, weekend |
| `tact_ttdnr_comphog_ds` | hours | Domestic shopping time, weekday |
| `tact_ttdnr_comphog_fds` | hours | Domestic shopping time, weekend |
| `tact_ttcnr_re_ds` | hours | Care education time, weekday |
| `tact_ttcnr_re_fds` | hours | Care education time, weekend |
| `tact_ttcnr_oac_health_ds` | hours | Care health time, weekday |
| `tact_ttcnr_oac_health_fds` | hours | Care health time, weekend |
| `tact_ttcnr_oac_work_ds` | hours | Care work time, weekday (always NA) |
| `tact_ttcnr_oac_work_fds` | hours | Care work time, weekend (always NA) |
| `tact_factor_sab` | ratio | Saturday normalization factor |
| `tact_factor_dom` | ratio | Sunday normalization factor |
| `tact_factor_ds` | ratio | Weekday redistribution factor |
