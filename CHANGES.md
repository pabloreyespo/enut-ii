# Changes to propagate to ENUT-I

All changes were applied to `data_processing/processing_functions.R`,
`data_processing/data_processing.R`, `data_processing/expenditures.R`,
`utils.R`, `exploration.R`, `models/`, and documentation files.

---

## 1. Variable renames

Apply as `replace_all` in every `.R` and `.r` file.

### Household composition

| Old | New |
|-----|-----|
| `n_menores6` | `n_menores_0_5` |
| `n_menores12` | `n_menores_6_11` |
| `n_men15` | `n_menores_0_14` |
| `n_menores18` | `n_menores_12_17` |
| `n_menores6_cut` | `n_menores_0_5_cut` |
| `n_menores12_cut` | `n_menores_6_11_cut` |
| `n_menores18_cut` | `n_menores_12_17_cut` |
| `n_tercera` | `n_tercera_edad` |
| `hay_tercera` | `hay_tercera_edad` |

Internal indicator variables (only in `new_variables_prefilter()`):

| Old | New |
|-----|-----|
| `menor6` | `menor_0_5` |
| `menor12` | `menor_6_11` |
| `men15` | `menor_0_14` |
| `menor18` | `menor_12_17` |

Expenditure model coefficient names (only in `expenditures.R`):

| Old | New |
|-----|-----|
| `bnmenores6_` | `bnmenores_0_5_` |
| `bnmenores12_` | `bnmenores_6_11_` |

### Sociodemographic

| Old | New |
|-----|-----|
| `horas_trabajo` | `horas_trabajo_contratadas` |
| `prop_hogar` | `prop_ing_hogar` |

### Python script (`gemelos_matriz.py`)

| Old | New |
|-----|-----|
| `"horas_trabajo"` | `"horas_trabajo_contratadas"` |

---

## 2. New variables

Add these to `new_variables_postfilter()` alongside `horas_trabajo_contratadas`:

```r
horas_trabajo_habituales = case_when(o22a > 0 ~ o22a, T ~ 0),
dias_trabajo_semana      = case_when(o22b > 0 ~ o22b, T ~ 0),
```

Add `n_tercera_edad` to the `composicion_hogar` selection vector (alongside `hay_tercera_edad`):

```r
composicion_hogar <- c(
  ...
  "n_tercera_edad",
  "hay_tercera_edad",
  ...
)
```

Add `horas_trabajo_habituales` and `dias_trabajo_semana` to `sociodemograficas`:

```r
sociodemograficas <- c(
  ...
  "horas_trabajo_contratadas",
  "horas_trabajo_habituales",
  "dias_trabajo_semana",
  ...
)
```

Add `ocup_form` to the `laborales` selection vector:

```r
laborales <- c("cae", "ocup_form", "cise", "ciuo_agrupada")
```

Update `impute_weekend()` to use the renamed variable:

```r
# old
mutate_at(dias_semana, ~ case_when((o22b < 5) & (o22b > 0) ~ . * o22b, T ~ . * 5))
# new
mutate_at(dias_semana, ~ case_when((dias_trabajo_semana < 5) & (dias_trabajo_semana > 0) ~ . * dias_trabajo_semana, T ~ . * 5))
```

Update `diagnostico_trabajo()` to use the renamed variables:

```r
# old
mutate(diferencia = t_to - o22c)
quantile(diagnostico[(diagnostico$t_to > 0) | (diagnostico$o22c > 0), ] ...)
ggplot(diagnostico, aes(x = o22a)) ...
ggplot(diagnostico, aes(x = o22b)) ...
ggplot(diagnostico, aes(x = o22c)) ...
# new: replace o22c -> horas_trabajo_contratadas, o22a -> horas_trabajo_habituales, o22b -> dias_trabajo_semana
```

Update intermediate file selection in `data_processing.R`:

```r
# old
o22a, o22b, o22c,
# new
horas_trabajo_habituales, dias_trabajo_semana, horas_trabajo_contratadas,
```

---

## 3. Labels to document

These are ENUT-specific label values; verify equivalents in ENUT-I before applying.

| Variable | Labels |
|----------|--------|
| `NSE` | 1 = Bajo, 2 = Medio, 3 = Alto |
| `ocup_form` | 1 = Formal, 2 = Informal |
| `bs1`-`bs3` | Ordinal 1-5: satisfaction scale |
| `bs4`-`bs9` | Ordinal 1-3: time adequacy scale |
| `bs10`-`bs11` | Ordinal 1-3: fairness of workload scale |
| `bs12`-`bs19` | Ordinal 1-5: caregiver burden frequency scale |

Time variable Spanish descriptions follow the ENUT module hierarchy:
`t_tcnr_*` = trabajo de cuidado no remunerado,
`t_tdnr_*` = trabajo doméstico no remunerado,
`t_tvaoh_*` = trabajo voluntario y ayuda a otros hogares,
`t_cpaf/ag_*` = cuidados personales,
`t_vsyo_*` = vida social y ocio,
`t_mcm_*` = medios de comunicación y masivos,
`t_tt*` = traslados.

---

## 4. English rename functions

Add to the bottom of `processing_functions.R`. The structure is:

```r
.rename_common_eng <- c(
  # identifiers
  id_person            = "id_persona",
  id_household         = "id_hog",
  # sample type
  is_worker            = "es_trabajador",
  is_family            = "es_familia",
  # diary
  weekday              = "dia_semana",
  weekend_day          = "dia_fin_semana",
  # household composition
  relationship_to_head = "parentesco",
  n_children_0_5       = "n_menores_0_5",
  n_children_6_11      = "n_menores_6_11",
  n_children_0_14      = "n_menores_0_14",
  n_youth_12_17        = "n_menores_12_17",
  n_underage           = "n_menores",
  n_adults             = "n_mayores",
  n_time_reporters     = "n_tiempo",
  n_workers            = "n_trabajadores",
  n_professionals      = "n_profesionales",
  n_elderly            = "n_tercera_edad",
  has_elderly          = "hay_tercera_edad",
  household_size       = "n_personas",
  mean_age             = "edad_promedio",
  has_children         = "tiene_hijos",
  in_couple            = "en_pareja",
  lives_with_partner   = "vive_pareja",
  # external support
  domestic_service     = "servicio_domestico",
  help_from_relatives  = "ayuda_cercanos",
  external_support     = "fuentes_externas",
  # sociodemographics
  female               = "sexo",
  age                  = "edad_anios",
  age_bracket          = "tramo_edad",
  ses                  = "NSE",
  education_level      = "nivel_escolaridad",
  in_education         = "estudia",
  employed             = "trabaja",
  contracted_hours     = "horas_trabajo_contratadas",
  usual_hours          = "horas_trabajo_habituales",
  work_days_per_week   = "dias_trabajo_semana",
  quintile             = "quintil",
  macrozone            = "macrozona",
  region               = "region_ord",
  region_name          = "glosa_region",
  income_share         = "prop_ing_hogar",
  # labor
  employment_status    = "cae",
  formality            = "ocup_form",
  occupation_group     = "ciuo_agrupada",
  # income
  inc_main_job         = "ing_ocuppal",
  inc_labor            = "ing_trab",
  inc_pension          = "ing_jub_aps",
  inc_group            = "ing_g",
  inc_household_total  = "ing_t_hogar",
  inc_household_pc     = "ing_t_pc",
  inc_group_personal   = "ing_gpp",
  inc_personal         = "ing_personal",
  household_income     = "ingreso_hogar",
  inc_person_week      = "income_person_week",
  # derived
  wage                 = "w",
  # expenditures
  food                 = "alimentos",
  clothing             = "vestimenta",
  utilities            = "cuentas",
  household_goods      = "hogar",
  health               = "salud",
  transportation       = "transporte",
  communications       = "comunicaciones",
  recreation           = "recreacion",
  education_exp        = "educacion",
  restaurants          = "restaurantes"
)

rename_to_english_25 <- function(data) {
  time_mapping <- c(
    t_paid_work              = "t_to",
    t_care_essential         = "t_tcnr_ce",
    t_care_education_related = "t_tcnr_re",
    t_care_other             = "t_tcnr_oac",
    t_domestic_meals         = "t_tdnr_psc",
    t_domestic_cleaning      = "t_tdnr_lv",
    t_domestic_laundry       = "t_tdnr_lrc",
    t_domestic_maintenance   = "t_tdnr_mrm",
    t_domestic_admin         = "t_tdnr_admnhog",
    t_domestic_shopping      = "t_tdnr_comphog",
    t_domestic_pets          = "t_tdnr_cmp",
    t_voluntary_community    = "t_tvaoh_tv",
    t_voluntary_households   = "t_tvaoh_oh",
    t_personal_care          = "t_cpaf_cp",
    t_meals                  = "t_cpag_comer",
    t_sleep                  = "t_cpag_dormir",
    t_education              = "t_ed",
    t_leisure_social         = "t_vsyo_csar",
    t_leisure_hobbies        = "t_vsyo_aa",
    t_media_reading          = "t_mcm_leer",
    t_media_audio            = "t_mcm_audio",
    t_media_video            = "t_mcm_video",
    t_media_computer         = "t_mcm_computador",
    t_commute1               = "t_tt1",
    t_commute2               = "t_tt2"
  )
  swb_mapping <- setNames(paste0("bs", 1:19), paste0("swb", 1:19))
  data %>% dplyr::rename(!!!c(.rename_common_eng, time_mapping, swb_mapping))
}

rename_to_english_11 <- function(data) {
  swb_mapping <- setNames(paste0("bs", 1:19), paste0("swb", 1:19))
  data %>% dplyr::rename(!!!c(.rename_common_eng, swb_mapping))
}
```

The mapping has 65 common renames (`.rename_common_eng`), 25 time-activity renames (25G only), and 19 swb renames (both versions). Verify that each Spanish variable name in `.rename_common_eng` exists in ENUT-I before applying; the time-activity names (`t_tcnr_*`, `t_tdnr_*`, etc.) are ENUT-II module codes and will differ in ENUT-I.

Add to `data_processing.R` after the Spanish G saves:

```r
data25G_ENG <- rename_to_english_25(data25G)
data11G_ENG <- rename_to_english_11(data11G)
haven::write_dta(data25G_ENG, "data/enut-ii-25G-ENG.dta")
haven::write_dta(data11G_ENG, "data/enut-ii-11G-ENG.dta")
write_csv(data25G_ENG, "data/enut-ii-25G-ENG.csv")
write_csv(data11G_ENG, "data/enut-ii-11G-ENG.csv")
```

---

## 5. Documentation files

Create one `.R` file per dataset using roxygen2-style `\describe{\item{}{}}` blocks.
Pattern used in this project:

| File | Documents |
|------|-----------|
| `enut_ii.R` | `enut-ii-25G` (Spanish names, full descriptions) |
| `enut_ii_11G.R` | `enut-ii-11G` (Spanish names; time vars are aggregates of 25G) |
| `enut_ii_25G_ENG.R` | `enut-ii-25G-ENG` (English names; `@seealso` Spanish version) |
| `enut_ii_11G_ENG.R` | `enut-ii-11G-ENG` (English names; `@seealso` Spanish version) |

Key conventions:
- Income variables: weekly thousands CLP, IPC-deflated
- Time variables: weekly hours, normalized to 168
- `bs*` -> `swb*` in English versions
- `hogar` (expenditure) -> `household_goods` to avoid clash with `id_household`
- `educacion` (expenditure) -> `education_exp` to avoid clash with `t_education`
- `cise` kept as-is in both languages (international classification code)

---

## 6. CLAUDE.md pipeline structure

Replace the "Running Code" section with a numbered pipeline:
- Step 0a: `expenditures.R` (one-time, produces `gastos.csv`)
- Step 0b: `gemelos_matriz.py` (one-time, Python, produces twin matrix)
- Step 1: `data_processing.R` (main R pipeline, two sub-blocks)
- Step 2: `exploration.R` (optional, independent)
- Step 3: model estimation via `utils.R` + `models/*.R`
- Step 4: `prediction/*.R`

See `CLAUDE.md` in this project for the full template.
