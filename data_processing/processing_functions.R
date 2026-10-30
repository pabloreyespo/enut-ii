pkgs <- c(
  "ggplot2", "dplyr", "tidyr", "readr", "purrr", "tibble", "stringr",
  "forcats", "lubridate", "haven", "reshape2", "apollo"
)
invisible(lapply(pkgs, library, character.only = TRUE))

acts_corregidas <- c(
  # confiables:
  "t_to", # trabajo en la ocupacion, sin traslados (to3, to7) ni busqueda (to9)
  "t_to_js", # busqueda de trabajo o inicio de negocio (to9)
  "t_tcnr_ce", # cuidados escenciales
  "t_tcnr_re", # cuidados relativos a la eseñanza
  "t_tcnr_oac", # otros cuidados
  # "t_tcnr_0_4",
  # "t_tcnr_5_14",
  # "t_tcnr_nna",  #(niños y adolescentes)
  # "t_tcnr_15_65",
  # "t_tcnr_66",
  # "t_tcnr_psdf"
  "t_tdnr_psc",
  "t_tdnr_lv",
  "t_tdnr_lrc",
  "t_tdnr_mrm",
  "t_tdnr_admnhog",
  "t_tdnr_comphog",
  "t_tdnr_cmp",
  "t_tvaoh_tv", # voluntario a la comunidad
  "t_tvaoh_oh", # voluntario a otros hogares
  "t_cpaf_cp", # cuidados personales
  "t_cpag_comer",
  "t_cpag_dormir",
  "t_ed",
  "t_vsyo_csar", # convivencia social y actividades recreativas (vs1 + vs3)
  "t_vsyo_aa", # arte y aficiones (vs4 + vs5)
  "t_vsyo_ev", # asistencia como publico a eventos culturales o deportivos (vs2)
  "t_vsyo_dep", # practica de deporte o ejercicio fisico (vs6)
  "t_descanso", # descanso, meditacion o relajacion (vs11)

  "t_mcm_leer",
  "t_mcm_video",
  "t_mcm_audio",
  "t_mcm_computador",
  "t_tto", # traslados asociados a trabajo remunerado
  "t_ted", # traslados asociados a educacion
  "t_tcpaf_cp", # traslados asociados a cuidados personales / salud
  "t_ttdnr_admnhog", # traslados asociados a administracion del hogar
  "t_ttdnr_comphog", # traslados asociados a compras del hogar
  "t_ttcnr_re", # traslados asociados a cuidados relativos a la ensenanza
  "t_ttcnr_oac_health", # traslados asociados a cuidados esenciales (salud)
  "t_ttcnr_oac_work" # traslados asociados a otros cuidados (trabajo)
)

# Alternative care time decomposition (NOT included in acts_corregidas to avoid double-counting)
# These variables represent care time by age group of care recipient:
#   t_tcnr_nna = t_tcnr_0_4 + t_tcnr_5_14
#   t_tcnr_nna + t_tcnr_15_65 + t_tcnr_66 + t_tcnr_psdf = t_tcnr_ce + t_tcnr_re + t_tcnr_oac
acts_care_alternatives <- c(
  "t_tcnr_0_4",
  "t_tcnr_5_14",
  "t_tcnr_nna",   # cuidado a ninas, ninos y adolescentes (0-14)
  "t_tcnr_15_65",
  "t_tcnr_66",
  "t_tcnr_psdf"
)

# Base care alternative variables (all come from raw data)
# These need _ds/_fds processing through the pipeline
acts_care_alt_base <- c(
  "t_tcnr_0_4",
  "t_tcnr_5_14",
  "t_tcnr_nna",
  "t_tcnr_15_65",
  "t_tcnr_66",
  "t_tcnr_psdf"
)

# Component variables used in tact numerators
# These need _sab/_dom imputation so tact_fds uses imputed weekend values
# tact_component_vars_nonwork need weekday redistribution (part of redistributed activities)
# tact_component_vars_work does NOT need weekday redistribution (part of t_to which is excluded)
tact_component_vars <- c(
  "to5_t",    # work commute numerator (part of t_to, NOT redistributed)
  "ed4_t",    # education commute numerator
  "cp9_t",    # personal care / health commute numerator
  "td16_t",   # domestic admin commute numerator
  "td22_t",   # domestic shopping commute numerator
  "tc16_t",   # care education numerator
  "tc22_t"    # care essential / health numerator
)
tact_component_vars_nonwork <- tact_component_vars[tact_component_vars != "to5_t"]

t_agregados <- c(
  "t_paid_work",
  "t_job_search",
  "t_domestic_work",
  "t_care_work",
  "t_unpaid_voluntary",
  "t_education",
  "t_leisure",
  "t_rest",
  "t_personal_care",
  "t_meals",
  "t_sleep",
  "t_commute"
)

t_agregados_new <- c(
  "Tw",
  "Tf_social",
  "Tf_events",
  "Tf_hobbies",
  "Tf_sports",
  "Tf_read",
  "Tf_listen",
  "Tf_watch",
  "Tf_computer",
  "Tf_rest",
  "Tc_meals",
  "Tc_sleep",
  "Tc_other"
)

tact_vars <- c(
  "tact_tto_ds", "tact_tto_fds",
  "tact_ted_ds", "tact_ted_fds",
  "tact_tcpaf_cp_ds", "tact_tcpaf_cp_fds",
  "tact_ttdnr_admnhog_ds", "tact_ttdnr_admnhog_fds",
  "tact_ttdnr_comphog_ds", "tact_ttdnr_comphog_fds",
  "tact_ttcnr_re_ds", "tact_ttcnr_re_fds",
  "tact_ttcnr_oac_health_ds", "tact_ttcnr_oac_health_fds",
  "tact_ttcnr_oac_work_ds", "tact_ttcnr_oac_work_fds",
  "tact_factor_sab", "tact_factor_dom", "tact_factor_ds"
)

identificadores <- c("id_persona", "id_hog")
composicion_hogar <- c(
  "parentesco",
  "n_menores_0_4",
  "n_menores_5_14",
  "n_nna",
  "n_menores_18",
  "n_personas_15_65",
  "n_mayores",
  "n_tiempo",
  "n_trabajadores",
  "n_profesionales",
  "n_tercera_edad",
  "hay_tercera_edad",
  "n_personas",
  "n_15_mas",
  "edad_promedio",
  "tiene_hijos",
  "en_pareja",
  "vive_pareja"
)

sociodemograficas <- c(
  "sexo",
  "edad_anios",
  "tramo_edad",
  "NSE",
  "cut",
  "nivel_escolaridad",
  "estudia",
  "trabaja",
  "horas_trabajo_contratadas",
  "horas_trabajo_habituales",
  "dias_trabajo_semana",
  "quintil",
  "macrozona",
  "region_ord",
  "glosa_region",
  "prop_ing_hogar"
)

laborales <- c("teletrabaja", "jornada_laboral", "cae", "ocup_form", "cise", "ciuo_agrupada")
proveedores_externos <- c("servicio_domestico", "ayuda_cercanos", "fuentes_externas")
ingresos <- c("ing_ocuppal", "ing_trab", "ing_jub_aps", "ing_otros", "ing_g", "ing_t_hogar", "ing_t_pc", "ing_gpp", "ing_personal", "ingreso_hogar", "income_person_week", "I")
tipo_muestra <- c("es_trabajador", "es_familia")

new_variables_prefilter <- function(data) {
  # OJO, DEBEN ENTRAR TODOS INDEPENDIENTE DE SI DECLARAN TIEMPO O NO
  act_s_domestico <- c("a6", "a14", "a24", "a32", "a1", "a19")
  recibe_servicio_domestico <- c("a1", "a19")

  # INFORMACIÓN DEL INDIVIDUO
  data <- data %>%
    mutate(
      menor_edad = case_when(edad < 18 ~ 1, TRUE ~ 0),
      menor_0_4 = case_when(edad <= 4 ~ 1, TRUE ~ 0),
      menor_5_14 = case_when(edad >= 5 & edad <= 14 ~ 1, TRUE ~ 0),
      menor_0_14 = case_when(edad <= 14 ~ 1, TRUE ~ 0),
      persona_15_65 = case_when(edad >= 15 & edad <= 65 ~ 1, TRUE ~ 0),
      menor25 = case_when(edad <= 25 ~ 1, TRUE ~ 0),
      mayor_edad = case_when(edad >= 18 ~ 1, TRUE ~ 0),
      tercera_edad = case_when(edad >= 60 ~ 1, TRUE ~ 0),
      nivel_escolaridad = nivel_educ,
      # comuna (CUT): varunit = "2023" + region (3) + provincia (1) + comuna (2) + conglomerado (4)
      cut = as.integer(substr(varunit, 5, 10)),
      # (1: 0,1; 2: 2,3,4; 3: 5,6,7,9; 4:8, 5:10,11,12)

      # tiene_hijos = case_when(t11_1_3 < 6 ~ 1, TRUE ~ 0),
      # trabaja     = case_when(k11_1_1 == 1 ~ 1, k13_1_1 == 1 ~ 1, TRUE ~ 0),
      trabaja = case_when(o1 == 1 ~ 1, TRUE ~ 0),
      teletrabaja = case_when(to2_v_ds == 1 | to2_v_fds == 1 ~ 1, TRUE ~ 0),
      jornada_laboral = jor_to,
      en_pareja = case_when(c7 <= 3 ~ 1, TRUE ~ 0),
      vive_pareja = case_when(c8a == 1 ~ 1, TRUE ~ 0),
      dia_semana = case_when(
        cd1 == "Lunes" ~ 1,
        cd1 == "Martes" ~ 2,
        cd1 == "Miercoles" ~ 3,
        cd1 == "Jueves" ~ 4,
        T ~ 5,
      ),
      dia_fin_semana = case_when(
        cd14 == "Sabado" ~ 6,
        T ~ 7
      )
    ) %>%
    group_by(id_hog) %>%
    mutate(tiene_hijos = case_when(
      sum(pco == 3) > 0 & pco <= 2 ~ 1, T ~ 0
    )) %>%
    ungroup()


  # COMPOSICIÓN DEL HOGAR
  data <- data %>%
    group_by(id_hog) %>%
    mutate(
      n_menores_18 = sum(menor_edad, na.rm = TRUE),
      n_menores_0_4 = sum(menor_0_4, na.rm = TRUE),
      n_menores_5_14 = sum(menor_5_14, na.rm = TRUE),
      n_nna = sum(menor_0_14, na.rm = TRUE),
      n_personas_15_65 = sum(persona_15_65, na.rm = TRUE),
      n_menores_1825 = sum(menor25, na.rm = TRUE),
      n_mayores = sum(mayor_edad, na.rm = TRUE),
      n_tercera_edad = sum(tercera_edad, na.rm = TRUE),
      n_tiempo = sum(tiempo, na.rm = T),
      n_trabajadores = sum(trabaja, na.rm = T),
      n_profesionales = sum(nivel_escolaridad >= 4, na.rm = TRUE),
      edad_promedio = mean(edad),
      n_15_mas = sum(edad >= 15, na.rm = TRUE)
    ) %>%
    mutate(
      # at least one member aged 60+ other than the respondent
      hay_tercera_edad = case_when(n_tercera_edad - tercera_edad >= 1 ~ 1, T ~ 0),
      n_personas = max(hhsize, na.rm = T)
    ) %>%
    ungroup()

  # AYUDAS QUE RECIBE EL HOGAR
  data <- data %>%
    mutate_at(act_s_domestico, ~ replace_na(., 0)) %>%
    mutate_at(act_s_domestico, ~ ifelse(. == 96, 0, .)) %>%
    group_by(id_hog) %>%
    mutate(
      servicio_domestico = abs(max(a1) - 2),
      ayuda_cercanos = abs(max(a19) - 2)
    ) %>%
    mutate(fuentes_externas = case_when(
      servicio_domestico == 1 | ayuda_cercanos == 1 ~ 1, T ~ 0
    )) %>%
    ungroup()

  # PREPROCESAMIENTO DE INGRESOS
  data <- data %>%
    # filter(tiempo == 1) %>% # será relevante desaserme de las personas que no declaran tiempo antes?
    mutate_at(
      c("ing_ocuppal", "ing_trab", "ing_jub_aps", "ing_t_hogar", "ing_t_pc"),
      ~ replace_na(., 0)
    ) %>%
    # rents (y7) and transfers from outside the household such as child support (y8) belong to
    # whoever receives them; -88/-96/-99 are don't know, missing and no answer
    mutate(ing_otros = ingreso_valido(y7b) + ingreso_valido(y8b)) %>%
    repartir_ingreso(hogar = "id_hog", edad = "edad") %>%
    arrange(ingreso_hogar) %>%
    mutate(percentil = cumsum(fe_ch) / sum(fe_ch)) %>%
    mutate(quintil = case_when(
      percentil <= 0.20000001 ~ 1,
      percentil <= 0.40000001 ~ 2,
      percentil <= 0.60000001 ~ 3,
      percentil <= 0.80000001 ~ 4,
      T ~ 5
    )) %>%
    dplyr::select(-c(percentil, fe_ch)) %>%
    mutate_at(c("prop_ing_hogar", "ingreso_hogar", "ing_gpp", "income_person_week", "ingreso_personal"), ~ replace_na(., 0)) %>%
    mutate(I = ing_jub_aps + ing_otros + ing_g)

  return(data)
}

ingreso_valido <- function(x) {
  x <- as.numeric(x)
  ifelse(is.na(x) | x < 0, 0, x)
}

# Splits household income among members. Own income = labor + pension + ing_otros. The household
# residual ing_g (ing_t_hogar minus own income, or the one the survey provides) goes in proportion
# to own income; if nobody in the household has own income, in equal parts among members aged 15+
# (everyone if there are none). ing_gpp = ing_otros + the residual share, so
# ingreso_personal = ing_trab + ing_jub_aps + ing_gpp adds up to the household income.
repartir_ingreso <- function(data, hogar, edad) {
  if (!"ing_otros" %in% names(data)) data$ing_otros <- 0
  data <- data %>%
    mutate(
      ingresos_propios = ing_trab + ing_jub_aps + ing_otros,
      .adulto = as.numeric(coalesce(.data[[edad]] >= 15, FALSE))
    ) %>%
    group_by(across(all_of(hogar)))
  if (!"ing_g" %in% names(data)) {
    data <- data %>% mutate(ing_g = (sum(ing_t_hogar) - sum(ingresos_propios)) * (ing_t_hogar > 0))
  }
  data %>%
    mutate(
      .s_prop = sum(ingresos_propios),
      .s_g = sum(ing_g),
      .n_adultos = sum(.adulto),
      .peso = case_when(
        .s_prop > 0 ~ ingresos_propios / .s_prop,
        .n_adultos > 0 ~ .adulto / .n_adultos,
        TRUE ~ 1 / n()
      ),
      ingreso_hogar = .s_prop + .s_g,
      ing_gpp = ing_otros + .s_g * .peso,
      income_person_week = (sum(ing_trab) + sum(ing_jub_aps) + ing_gpp) / n(),
      ingreso_personal = ing_trab + ing_jub_aps + ing_gpp,
      prop_ing_hogar = if (sum(ingreso_personal) > 0) ingreso_personal / sum(ingreso_personal) else 0
    ) %>%
    ungroup() %>%
    dplyr::select(-c(.adulto, .s_prop, .s_g, .n_adultos, .peso))
}

na_completion <- function(data) {
  # Commute components grouped by macro-activity (from clasificacion_traslado.csv)
  act_traslados <- list(
    to          = c("to3_t", "to7_t"),            # asociados a trabajo remunerado (t_to)
    ed          = c("ed2_t", "ed5_t"),            # asociados a educacion (t_ed)
    cpaf_cp     = c("cp7_t", "cp10_t"),           # asociados a cuidados personales / salud (t_cpaf_cp)
    tdnr_admnhog = c("td14_t", "td17_t"),         # asociados a administracion del hogar (t_tdnr_admnhog)
    tdnr_comphog = c("td20_t", "td23_t"),         # asociados a compras del hogar (t_tdnr_comphog)
    tcnr_re     = c("tc12_t", "tc15_t"),          # asociados a cuidados relativos a la ensenanza (t_tcnr_re)
    tcnr_oac_health  = c("tc21_t", "tc25_t"),    # asociados a cuidados esenciales (t_tcnr_ce)
    tcnr_oac_work    = c("tc31_t", "tc34_t")           # asociados a otros cuidados (t_tcnr_oac)
  )
  # Backward-compat flat vectors (kept for acts_ds/acts_fds processing)
  act_traslados_all <- unlist(act_traslados)
  act_traslados_all_ds <- paste0(act_traslados_all, "_ds")
  act_traslados_all_fds <- paste0(act_traslados_all, "_fds")
  vars_todos <- c("bs1")

  acts <- c(
    # confiables:
    "t_to",
    "cp1_t",

    # no confiables:
    "t_tcnr_ce", # cuidados escenciales
    "t_tcnr_re", # cuidados relativos a la eseñanza
    "t_tcnr_oac", # otros cuidados
    "t_tcnr_0_4", # cuidado a menores de 0-4 anos
    "t_tcnr_5_14", # cuidado a menores de 5-14 anos
    "t_tcnr_nna", # cuidado a ninas, ninos y adolescentes (0-14)
    "t_tcnr_15_65", # cuidado a personas de 15-65 anos
    "t_tcnr_66", # cuidado a personas de 66+ anos
    "t_tcnr_psdf", # cuidado a personas con discapacidad
    "t_tdnr_psc",
    "t_tdnr_lv",
    "t_tdnr_lrc",
    "t_tdnr_mrm",
    "t_tdnr_admnhog",
    "t_tdnr_comphog",
    "t_tdnr_cmp",
    "t_tvaoh_tv", # voluntario a la comunidad
    "t_tvaoh_oh", # voluntario a otros hogares
    "t_cpaf_cp", # cuidados personales
    "cp3_t", "cp4_t", "cp5_t", # crear t_cpag_comer
    "t_ed",
    "t_vsyo_csar", # convivencia social y actividades recreativas
    "t_vsyo_aa", # arte y aficiones
    "to9_t", # busqueda de trabajo (incluida en t_to)
    "vs2_t", # eventos como publico (fuera de t_vsyo_csar y t_vsyo_aa)
    "vs6_t", # deporte o ejercicio fisico (fuera de t_vsyo_csar y t_vsyo_aa)
    "vs11_t", # descanso (fuera de t_vsyo y t_mcm)
    "vs7_t",
    "vs8_t",
    "vs9_t",
    "vs10_t"
  )

  acts_ds <- paste0(c(acts, act_traslados_all), "_ds")
  acts_fds <- paste0(c(acts, act_traslados_all), "_fds")

  data <- data %>%
    # TODO reemplazar los 96 con 0
    filter(tiempo == 1) %>%
    mutate(across(
      c(edad, sexo), function(x) ifelse(is.na(x), 85, x)
    )) %>%
    filter(if_all(c("cp1_t_ds", "cp1_t_fds", "t_to_ds", "t_to_fds"), ~ !.x %in% 96)) %>%
    filter(!is.na(nivel_educ)) %>%
    # filter(if_all(n_linea_p:te_ayuda_cercanos, ~ !.x  %in% 96)) %>%
    # filter(l110_1_1 != 1 & l122_1_1 != 1) %>% # en cama enfermos
    # filter(l111_1_1 != 1 & l123_1_1 != 1) %>%# pasaron cosas fuera de lo común
    # filter(m14_1_1 != 2 & m14_2_1 != 2) %>% # huelga
    # filter(k42_1_1 != 6) %>%# no buscó trabajo por estar embarazada
    # filter(!(l15_1_1 == 2 & l16_1_1 == 2 & l17_1_1 == 2 & (k42_1_1 == 2 | k42_1_1 == 3 | k42_1_1 == 5))) %>% # los de vacaciones
    # filter(!(l117_1_1 == 2 & l118_1_1 == 2 & l119_1_1 == 2 & (k42_1_1 == 2 | k42_1_1 == 3 | k42_1_1 == 5))) %>%
    # filter(k15_1_1 != 1) %>%
    mutate_at(c(acts_ds, acts_fds), ~ ifelse(is.na(.), 0, .)) %>%
    mutate_at(c(acts_ds, acts_fds), ~ ifelse(. > 25, 0, .)) %>%
    mutate_at(c(acts_ds, acts_fds), ~ replace_na(., 0)) %>%
    mutate_at(c("o22a", "o22b", "o22c"), ~ ifelse(. == 96, 0, .)) %>%
    mutate_at(c("o22a", "o22b", "o22c"), ~ replace_na(., 0)) %>%
    filter(if_all(all_of(vars_todos), ~ !.x %in% 85)) %>%
    mutate(
      t_cpag_comer_ds = dplyr::select(., all_of(paste0(c("cp3", "cp4", "cp5"), "_t_ds"))) %>% rowSums(),
      t_cpag_comer_fds = dplyr::select(., all_of(paste0(c("cp3", "cp4", "cp5"), "_t_fds"))) %>% rowSums(),
      t_cpag_dormir_ds = cp1_t_ds,
      t_cpag_dormir_fds = cp1_t_fds,
    ) %>%
    # INE aggregates already contain their commutes and job search:
    # t_to = to3 + to5 + to7 + to9, t_ed = ed2 + ed4 + ed5 + ed7 + ed8,
    # t_tcnr_oac = ... + tc21 + tc25 + tc31 + tc34. Those components are
    # removed here because they are added back as t_tto, t_ted, t_ttcnr_* and
    # t_to_js below.
    mutate(
      t_to_js_ds = to9_t_ds,
      t_to_js_fds = to9_t_fds,
      t_to_ds = t_to_ds - to3_t_ds - to7_t_ds - to9_t_ds,
      t_to_fds = t_to_fds - to3_t_fds - to7_t_fds - to9_t_fds,
      t_ed_ds = t_ed_ds - ed2_t_ds - ed5_t_ds,
      t_ed_fds = t_ed_fds - ed2_t_fds - ed5_t_fds,
      t_vsyo_ev_ds = vs2_t_ds,
      t_vsyo_ev_fds = vs2_t_fds,
      t_vsyo_dep_ds = vs6_t_ds,
      t_vsyo_dep_fds = vs6_t_fds,
      t_descanso_ds = vs11_t_ds,
      t_descanso_fds = vs11_t_fds
    ) %>%
    mutate(
      t_cpaf_af_ds = t_cpaf_af_ds - t_cpag_dormir_ds - t_cpag_comer_ds,
      t_cpaf_af_fds = t_cpaf_af_fds - t_cpag_dormir_fds - t_cpag_comer_fds,
      t_cpaf_cp_ds = t_cpaf_cp_ds - cp7_t_ds - cp10_t_ds,
      t_cpaf_cp_fds = t_cpaf_cp_fds - cp7_t_fds - cp10_t_fds,
      t_tdnr_admnhog_ds = t_tdnr_admnhog_ds - td14_t_ds - td17_t_ds,
      t_tdnr_admnhog_fds = t_tdnr_admnhog_fds - td14_t_fds - td17_t_fds,
      t_tdnr_comphog_ds = t_tdnr_comphog_ds - td20_t_ds - td23_t_ds,
      t_tdnr_comphog_fds = t_tdnr_comphog_fds - td20_t_fds - td23_t_fds,
      t_tcnr_re_ds = t_tcnr_re_ds - tc12_t_ds - tc15_t_ds,
      t_tcnr_re_fds = t_tcnr_re_fds - tc12_t_fds - tc15_t_fds,
      # tc21/tc25 (health) and tc31/tc34 (work) commutes are inside t_tcnr_oac
      t_tcnr_oac_ds = t_tcnr_oac_ds - tc21_t_ds - tc25_t_ds - tc31_t_ds - tc34_t_ds,
      t_tcnr_oac_fds = t_tcnr_oac_fds - tc21_t_fds - tc25_t_fds - tc31_t_fds - tc34_t_fds,

      t_tto_ds = dplyr::select(., all_of(paste0(act_traslados$to, "_ds"))) %>% rowSums(na.rm = T),
      t_tto_fds = dplyr::select(., all_of(paste0(act_traslados$to, "_fds"))) %>% rowSums(na.rm = T),
      t_ted_ds = dplyr::select(., all_of(paste0(act_traslados$ed, "_ds"))) %>% rowSums(na.rm = T),
      t_ted_fds = dplyr::select(., all_of(paste0(act_traslados$ed, "_fds"))) %>% rowSums(na.rm = T),
      t_tcpaf_cp_ds = dplyr::select(., all_of(paste0(act_traslados$cpaf_cp, "_ds"))) %>% rowSums(na.rm = T),
      t_tcpaf_cp_fds = dplyr::select(., all_of(paste0(act_traslados$cpaf_cp, "_fds"))) %>% rowSums(na.rm = T),
      t_ttdnr_admnhog_ds = dplyr::select(., all_of(paste0(act_traslados$tdnr_admnhog, "_ds"))) %>% rowSums(na.rm = T),
      t_ttdnr_admnhog_fds = dplyr::select(., all_of(paste0(act_traslados$tdnr_admnhog, "_fds"))) %>% rowSums(na.rm = T),
      t_ttdnr_comphog_ds = dplyr::select(., all_of(paste0(act_traslados$tdnr_comphog, "_ds"))) %>% rowSums(na.rm = T),
      t_ttdnr_comphog_fds = dplyr::select(., all_of(paste0(act_traslados$tdnr_comphog, "_fds"))) %>% rowSums(na.rm = T),
      t_ttcnr_re_ds = dplyr::select(., all_of(paste0(act_traslados$tcnr_re, "_ds"))) %>% rowSums(na.rm = T),
      t_ttcnr_re_fds = dplyr::select(., all_of(paste0(act_traslados$tcnr_re, "_fds"))) %>% rowSums(na.rm = T),
      t_ttcnr_oac_health_ds = dplyr::select(., all_of(paste0(act_traslados$tcnr_oac_health, "_ds"))) %>% rowSums(na.rm = T),
      t_ttcnr_oac_health_fds = dplyr::select(., all_of(paste0(act_traslados$tcnr_oac_health, "_fds"))) %>% rowSums(na.rm = T),
      t_ttcnr_oac_work_ds = dplyr::select(., all_of(paste0(act_traslados$tcnr_oac_work, "_ds"))) %>% rowSums(na.rm = T),
      t_ttcnr_oac_work_fds = dplyr::select(., all_of(paste0(act_traslados$tcnr_oac_work, "_fds"))) %>% rowSums(na.rm = T),
      
      t_mcm_leer_ds = dplyr::select(., all_of(c("vs7_t_ds"))) %>% rowSums(na.rm = T),
      t_mcm_video_ds = dplyr::select(., all_of(c("vs8_t_ds"))) %>% rowSums(na.rm = T),
      t_mcm_audio_ds = dplyr::select(., all_of(c("vs9_t_ds"))) %>% rowSums(na.rm = T),
      t_mcm_computador_ds = dplyr::select(., all_of(c("vs10_t_ds"))) %>% rowSums(na.rm = T),
      t_mcm_leer_fds = dplyr::select(., all_of(c("vs7_t_fds"))) %>% rowSums(na.rm = T),
      t_mcm_video_fds = dplyr::select(., all_of(c("vs8_t_fds"))) %>% rowSums(na.rm = T),
      t_mcm_audio_fds = dplyr::select(., all_of(c("vs9_t_fds"))) %>% rowSums(na.rm = T),
      t_mcm_computador_fds = dplyr::select(., all_of(c("vs10_t_fds"))) %>% rowSums(na.rm = T)
    ) %>%
    mutate(
      t_total_ds = dplyr::select(., all_of(paste0(acts_corregidas, "_ds"))) %>% rowSums(na.rm = T),
      t_total_fds = dplyr::select(., all_of(paste0(acts_corregidas, "_fds"))) %>% rowSums(na.rm = T),
    ) %>%
    mutate(sexo = abs(sexo - 1))

  return(data)
}

outlier_detection_Vallejo <- function(data) {
  data["dias_normal"] <- TRUE
  data <- data %>%
    # group_by(años_escolaridad, k11_1_1, rango_edad,sexo) %>%
    group_by(quintil, trabaja, tramo_edad, sexo) %>%
    mutate(
      xi_ds = mean(t_total_ds), sd_ds = sd(t_total_ds),
      xi_fds = mean(t_total_fds), sd_fds = sd(t_total_fds)
    ) %>%
    mutate(gamma_ds = sd_ds / xi_ds, gamma_fds = sd_fds / xi_fds) %>%
    mutate(
      lower_limit_ds = xi_ds - sd_ds + gamma_ds / 2,
      upper_limit_ds = xi_ds + sd_ds,
      lower_limit_fds = xi_fds - sd_fds + gamma_fds / 2,
      upper_limit_fds = xi_fds + sd_fds
    ) %>%
    ungroup() %>%
    mutate(dias_normal = case_when(
      dias_normal == FALSE ~ FALSE,
      t_total_ds < lower_limit_ds | t_total_ds > upper_limit_ds ~ FALSE,
      t_total_fds < lower_limit_fds | t_total_fds > upper_limit_fds ~ FALSE,
      TRUE ~ TRUE
    )) %>%
    filter(dias_normal == TRUE)
  return(data)
}

new_variables_postfilter <- function(data) {
  ingresos <- c(
    "ing_ocuppal", "ing_trab", "ing_jub_aps", "ing_otros", "ing_g", "ing_t_hogar", "ing_t_pc",
    "ing_gpp", "ing_personal", "ingreso_hogar", "income_person_week"
  )

  data <- data %>%
    mutate(
      horas_trabajo_contratadas = case_when(o22c > 0 ~ o22c, T ~ 0),
      horas_trabajo_habituales   = case_when(o22a > 0 ~ o22a, T ~ 0),
      dias_trabajo_semana       = case_when(o22b > 0 ~ o22b, T ~ 0),
      macrozona = case_when(
        region_ord <= 5 ~ "norte",
        region_ord == 7 ~ "metropolitana",
        region_ord <= 11 ~ "centro",
        TRUE ~ "sur"
      ),
      edad_anios = edad,
      tramo_edad = case_when(
        tr_edad == 1 ~ "12-24",
        tr_edad == 2 ~ "25-44",
        tr_edad == 3 ~ "45-65",
        T ~ "66+"
      ), # 12-24, 25-45, 46-65, 66+
      estudia = abs(abs(e4 - 1) - 1),
      n_menores_18 = case_when(n_menores_18 <= 3 ~ n_menores_18, T ~ 4),
      n_mayores = case_when(n_mayores <= 5 ~ n_mayores, T ~ 6),
      nivel_escolaridad = case_when( # todos los niveles son completos, excepto el inicial
        nivel_escolaridad == 1 ~ "ninguna",
        nivel_escolaridad == 2 ~ "primaria",
        nivel_escolaridad == 3 ~ "secundaria",
        nivel_escolaridad == 4 ~ "técnica",
        T ~ "universitaria"
      ),
      parentesco = pco,
      ing_personal = ing_trab + ing_jub_aps + ing_gpp
    ) %>%
    mutate_at(ingresos, ~ . / 1000 / 4) %>% # /654
    mutate(ing_personal = round(ing_personal, 2))


  data <- data %>% mutate(
    cae = cae, # cae es igual a la anter
    cise = cise,
    ciuo_agrupada = ciuo_agrupada
  )

  cae_id <- c("Menor de 15 años" = 1, "Ocupada(o)" = 2, "Desocupada(o)" = 3, "Inactiva(o)" = 4, "Sin clasificar" = 96)
  data$cae <- names(cae_id)[match(data$cae, cae_id)]

  return(data)
}

data_to168hours <- function(data) {
  act_confiable <- c("t_to", "t_cpag_dormir", "t_tto", "t_ted", "t_tcpaf_cp", "t_ttdnr_admnhog", "t_ttdnr_comphog", "t_ttcnr_re", "t_ttcnr_oac_health", "t_ttcnr_oac_work")
  act_no_confiable <- acts_corregidas[!acts_corregidas %in% act_confiable]

  data <- data %>%
    # mutate_at(c(act_confiable, act_no_confiable), ~ifelse(. <= 1/4,0 ,.)) %>%
    mutate(
      t_confiable = dplyr::select(., all_of(act_confiable)) %>% rowSums(na.rm = TRUE),
      t_no_confiable = dplyr::select(., all_of(act_no_confiable)) %>% rowSums(na.rm = TRUE)
    ) %>%
    mutate(t_total = t_confiable + t_no_confiable) %>%
    filter(t_confiable <= 168) %>%
    mutate(mult_no_confiable = (168 - t_confiable) / (t_total - t_confiable)) %>%
    mutate_at(act_no_confiable, ~ . * mult_no_confiable) %>%
    # Scale care alternatives by the same proportion as no-confiable activities
    mutate_at(acts_care_alt_base, ~ . * mult_no_confiable) %>%
    mutate(
      t_total = dplyr::select(., all_of(c(act_confiable, act_no_confiable))) %>%
        rowSums(na.rm = TRUE)
    )

  return(data)
}

impute_weekend <- function(data, twin_matrix) {
  # Ensure twin_matrix is a proper matrix (not a tibble/data.frame) for
  # matrix-style subsetting with logical vectors on both rows and columns.
  if (!is.matrix(twin_matrix)) {
    twin_matrix <- as.matrix(twin_matrix)
  }

  # Ensure dimensions match: twin_matrix must be nrow(data) x nrow(data)
  nr <- nrow(data)
  if (nrow(twin_matrix) != nr || ncol(twin_matrix) != nr) {
    stop(paste0(
      "twin_matrix dimensions (", nrow(twin_matrix), " x ", ncol(twin_matrix),
      ") do not match data rows (", nr, ")"
    ))
  }

  acts <- list(
    "6" = paste0(acts_corregidas, "_sab"),
    "7" = paste0(acts_corregidas, "_dom")
  )

  finsemana <- paste0(acts_corregidas, "_fds")
  dias_semana <- paste0(acts_corregidas, "_ds")

  semana_completa <- acts_corregidas

  # Care alternative variables (processed in parallel, same proportions)
  care_acts <- list(
    "6" = paste0(acts_care_alt_base, "_sab"),
    "7" = paste0(acts_care_alt_base, "_dom")
  )
  care_fds <- paste0(acts_care_alt_base, "_fds")
  care_ds <- paste0(acts_care_alt_base, "_ds")
  care_semana_completa <- acts_care_alt_base

  # Tact component variables (processed in parallel, same proportions)
  tact_acts <- list(
    "6" = paste0(tact_component_vars, "_sab"),
    "7" = paste0(tact_component_vars, "_dom")
  )
  tact_fds <- paste0(tact_component_vars, "_fds")
  tact_ds <- paste0(tact_component_vars, "_ds")
  tact_semana_completa <- tact_component_vars

  for (i in 6:7) {
    mask <- data$dia_fin_semana == i
    finde <- unlist(acts[as.character(i)])

    data[mask, finde] <- data[mask, finsemana]
    suma <- rowSums(twin_matrix[!mask, mask])
    data[!mask, finde] <- as.matrix(twin_matrix[!mask, mask]) %*% as.matrix(data[mask, finsemana])
    data[!mask, finde] <- sweep(data[!mask, finde], 1, suma, "/")

    # Twin matrix imputation for care alternatives
    care_finde <- unlist(care_acts[as.character(i)])
    data[mask, care_finde] <- data[mask, care_fds]
    data[!mask, care_finde] <- as.matrix(twin_matrix[!mask, mask]) %*% as.matrix(data[mask, care_fds])
    data[!mask, care_finde] <- sweep(data[!mask, care_finde], 1, suma, "/")

    # Twin matrix imputation for tact component variables
    tact_finde <- unlist(tact_acts[as.character(i)])
    data[mask, tact_finde] <- data[mask, tact_fds]
    data[!mask, tact_finde] <- as.matrix(twin_matrix[!mask, mask]) %*% as.matrix(data[mask, tact_fds])
    data[!mask, tact_finde] <- sweep(data[!mask, tact_finde], 1, suma, "/")
  }

  sabados <- unlist(acts["6"])
  domingos <- unlist(acts["7"])
  care_sab <- unlist(care_acts["6"])
  care_dom <- unlist(care_acts["7"])
  tact_sab <- unlist(tact_acts["6"])
  tact_dom <- unlist(tact_acts["7"])
  data_post <- data %>%
    # mutate_at(sabados , ~ifelse(. <= 1/12,0 ,.)) %>% # todo tiempo al que se le dedique menos de 5 minutos
    # mutate_at(domingos , ~ifelse(. <= 1/12,0 ,.)) %>%  # todo tiempo al que se le dedique menos de 5 minutos
    mutate(
      sum_sabados = dplyr::select(., all_of(sabados)) %>% rowSums(na.rm = TRUE),
      sum_domingos = dplyr::select(., all_of(domingos)) %>% rowSums(na.rm = TRUE)
    ) %>%
    mutate_at(sabados, ~ . * 24 / sum_sabados) %>%
    mutate_at(domingos, ~ . * 24 / sum_domingos) %>%
    # Scale care alternatives using the same proportions as acts_corregidas
    mutate_at(care_sab, ~ . * 24 / sum_sabados) %>%
    mutate_at(care_dom, ~ . * 24 / sum_domingos) %>%
    # Scale tact components using the same proportions as acts_corregidas
    mutate_at(tact_sab, ~ . * 24 / sum_sabados) %>%
    mutate_at(tact_dom, ~ . * 24 / sum_domingos) %>%
    mutate(
      sum_sabados = dplyr::select(., all_of(sabados)) %>% rowSums(na.rm = TRUE),
      sum_domingos = dplyr::select(., all_of(domingos)) %>% rowSums(na.rm = TRUE)
    ) %>%
    mutate_at(dias_semana, ~ case_when((dias_trabajo_semana < 5) & (dias_trabajo_semana > 0) ~ . * dias_trabajo_semana, T ~ . * 5)) %>%
    mutate_at(care_ds, ~ case_when((dias_trabajo_semana < 5) & (dias_trabajo_semana > 0) ~ . * dias_trabajo_semana, T ~ . * 5)) %>%
    mutate_at(tact_ds, ~ case_when((dias_trabajo_semana < 5) & (dias_trabajo_semana > 0) ~ . * dias_trabajo_semana, T ~ . * 5))

  temp <- data_post[, "t_to_ds"]
  data_post[, "t_to_ds"] <- 0
  non_t_to_sums <- rowSums(data_post[, dias_semana])
  data_post[, dias_semana] <- sweep(data_post[, dias_semana], 1, non_t_to_sums, "/")
  data_post[, dias_semana] <- sweep(data_post[, dias_semana], 1, as.numeric(unlist(24 * 5 - temp)), "*")
  data_post[, "t_to_ds"] <- temp

  # Redistribute care alternative weekday time using the same proportional factor
  weekday_factor <- as.numeric(unlist(24 * 5 - temp)) / non_t_to_sums
  data_post[, care_ds] <- sweep(data_post[, care_ds], 1, weekday_factor, "*")

  # Redistribute tact non-work component weekday time (to5_t is part of t_to, NOT redistributed)
  tact_ds_nonwork <- paste0(tact_component_vars_nonwork, "_ds")
  data_post[, tact_ds_nonwork] <- sweep(data_post[, tact_ds_nonwork], 1, weekday_factor, "*")

  data_post[, semana_completa] <- data_post[, dias_semana] + data_post[, sabados] + data_post[, domingos]
  data_post[, care_semana_completa] <- data_post[, care_ds] + data_post[, care_sab] + data_post[, care_dom]
  data_post[, tact_semana_completa] <- data_post[, tact_ds] + data_post[, tact_sab] + data_post[, tact_dom]
  data_post[, "t_total"] <- rowSums(data_post[, semana_completa])

  # Store normalization factors for downstream use
  data_post[, "tact_factor_sab"] <- as.numeric(unlist(24 / rowSums(data_post[, sabados])))
  data_post[, "tact_factor_dom"] <- as.numeric(unlist(24 / rowSums(data_post[, domingos])))
  data_post[, "tact_factor_ds"] <- as.numeric(unlist(weekday_factor))

  return(data_post)
}

diagnostico_trabajo <- function(data, plots, intercuartil) {
  diagnostico <- data %>%
    mutate(diferencia = t_to - horas_trabajo_contratadas)

  cat("Número inicial de filas", nrow(diagnostico), end = "\n")

  if (plots) {
    ggplot(diagnostico, aes(x = horas_trabajo_habituales)) +
      geom_histogram(binwidth = 1)
    ggplot(diagnostico, aes(x = dias_trabajo_semana)) +
      geom_histogram(binwidth = 1)
    ggplot(diagnostico, aes(x = horas_trabajo_contratadas)) +
      geom_histogram(binwidth = 1)
    ggplot(diagnostico, aes(x = t_to)) +
      geom_histogram(binwidth = 1)
    ggplot(diagnostico, aes(x = diferencia)) +
      geom_histogram(binwidth = 1)
  }

  if (intercuartil) {
    cuantiles <- quantile(diagnostico[(diagnostico$t_to > 0) | (diagnostico$horas_trabajo_contratadas > 0), ]$diferencia, c(0.25, 0.75))
    intercuantil <- cuantiles[2] - cuantiles[1]
    diagnostico <- diagnostico %>% filter(diferencia < cuantiles[2] + intercuantil, diferencia > cuantiles[1] - intercuantil)
    cat("Número final de filas", nrow(diagnostico))
    return(diagnostico)
  } else {
    diagnostico <- diagnostico %>% filter(diferencia < 10, diferencia > -10)
    cat("Número final de filas", nrow(diagnostico))
    return(diagnostico)
  }
}

filtrar_salarios_extremos <- function(data, k = 3, ingreso = "ing_trab", horas = "t_to") {
  # Among workers with a positive wage rate w = ingreso / horas, drops the rows whose
  # log w lies outside [Q1 - k IQR, Q3 + k IQR]. Rows without a positive w are kept.
  cat("Numero inicial de filas", nrow(data), "
")
  w <- data[[ingreso]] / data[[horas]]
  con_salario <- is.finite(w) & w > 0
  mantener <- !con_salario
  if (any(con_salario)) {
    log_w <- log(w[con_salario])
    q <- quantile(log_w, c(0.25, 0.75), names = FALSE)
    iqr <- q[2] - q[1]
    mantener[con_salario] <- log_w >= q[1] - k * iqr & log_w <= q[2] + k * iqr
  }
  data <- data[mantener, ]
  cat("Numero final de filas", nrow(data), "
")
  return(data)
}

adjust_working_hours <- function(data, normalize = TRUE) {
  act_ajustar <- acts_corregidas[!acts_corregidas %in% c("t_to")]
  data <- data %>% mutate(t_to = horas_trabajo_contratadas) # verificar con Astroza

  # if (normalize) {
  #   data <- data %>%
  #     mutate(t_ajustable = dplyr::select(.,act_ajustar) %>% rowSums(na.rm = TRUE)) %>%
  #     mutate(mult_ajuste= (168- tt)/(t_ajustable)) %>%
  #     mutate_at(act_ajustar, ~ .*mult_ajuste) %>%
  #     mutate(ttotal_ajuste = dplyr::select(., c("tt", act_ajustar))  %>% rowSums(na.rm = TRUE))
  # }

  return(data)
}

agregar_actividades <- function(data_post) {
  data_post <- data_post %>%
    mutate(
      Tw = t_to,
      Tf_social = t_vsyo_csar,
      Tf_events = t_vsyo_ev,
      Tf_hobbies = t_vsyo_aa,
      Tf_sports = t_vsyo_dep,
      Tf_read = t_mcm_leer,
      Tf_listen = t_mcm_audio,
      Tf_watch = t_mcm_video,
      Tf_computer = t_mcm_computador,
      Tf_rest = t_descanso,
      Tc_meals = t_cpag_comer,
      Tc_sleep = t_cpag_dormir,
      Tc_other = dplyr::select(., c(
        "t_to_js",
        "t_tdnr_psc", "t_tdnr_lv", "t_tdnr_lrc", "t_tdnr_mrm",
        "t_tdnr_admnhog", "t_tdnr_comphog", "t_tdnr_cmp",
        "t_tcnr_ce", "t_tcnr_re", "t_tcnr_oac",
        "t_tvaoh_tv", "t_tvaoh_oh",
        "t_cpaf_cp", "t_ed",
        "t_tto", "t_ted", "t_tcpaf_cp",
        "t_ttdnr_admnhog", "t_ttdnr_comphog",
        "t_ttcnr_re", "t_ttcnr_oac_health", "t_ttcnr_oac_work"
      )) %>% rowSums(na.rm = TRUE),
      t_paid_work = t_to, # free
      t_job_search = t_to_js,
      t_domestic_work = dplyr::select(., c(
        "t_tdnr_psc", "t_tdnr_lv", "t_tdnr_lrc", "t_tdnr_mrm",
        "t_tdnr_admnhog", "t_tdnr_comphog", "t_tdnr_cmp",
      )) %>%
        rowSums(na.rm = TRUE), # commited/free
      t_care_work = dplyr::select(., c("t_tcnr_ce", "t_tcnr_re", "t_tcnr_oac", )) %>%
        rowSums(na.rm = TRUE), # commited/free
      t_unpaid_voluntary = t_tvaoh_tv + t_tvaoh_oh, # free
      t_education = t_ed, # committed / free
      t_leisure = t_vsyo_csar + t_vsyo_ev + t_vsyo_aa + t_vsyo_dep + t_mcm_leer + t_mcm_video + t_mcm_audio + t_mcm_computador, # free
      t_rest = t_descanso,
      t_personal_care = t_cpaf_cp, # committed/free
      t_meals = t_cpag_comer, # committed/free
      t_sleep = t_cpag_dormir, # committed/free
      t_commute = t_tto + t_ted + t_tcpaf_cp + t_ttdnr_admnhog + t_ttdnr_comphog + t_ttcnr_re + t_ttcnr_oac_health + t_ttcnr_oac_work)

  data_post <- data_post %>%
    mutate(
      es_trabajador = case_when(trabaja == 1 & cae == "Ocupada(o)" & t_to > 0 ~ 1, T ~ 0),
      es_familia = case_when(trabaja == 1 & cae == "Ocupada(o)" & t_to > 0 & vive_pareja == 1 & tiene_hijos == 1 ~ 1, T ~ 0)
    )

  # Activity times for commute-associated activities (post-twin-imputation)
  # _ds uses weekday values, _fds uses imputed weekend (sab + dom) values
  # Normalization factors (tact_factor_*) are carried from impute_weekend
  data_post <- data_post %>%
    mutate(
      # Activity times (already normalized in impute_weekend)
      tact_tto_ds = to5_t_ds,
      tact_tto_fds = to5_t_sab + to5_t_dom,
      tact_ted_ds = ed4_t_ds,
      tact_ted_fds = ed4_t_sab + ed4_t_dom,
      tact_tcpaf_cp_ds = cp9_t_ds,
      tact_tcpaf_cp_fds = cp9_t_sab + cp9_t_dom,
      tact_ttdnr_admnhog_ds = td16_t_ds,
      tact_ttdnr_admnhog_fds = td16_t_sab + td16_t_dom,
      tact_ttdnr_comphog_ds = td22_t_ds,
      tact_ttdnr_comphog_fds = td22_t_sab + td22_t_dom,
      tact_ttcnr_re_ds = tc16_t_ds,
      tact_ttcnr_re_fds = tc16_t_sab + tc16_t_dom,
      tact_ttcnr_oac_health_ds = tc22_t_ds,
      tact_ttcnr_oac_health_fds = tc22_t_sab + tc22_t_dom,
      tact_ttcnr_oac_work_ds = NA_real_,
      tact_ttcnr_oac_work_fds = NA_real_
    )

  data25 <- data_post %>%
    dplyr::select(
      all_of(identificadores),
      all_of(tipo_muestra),
      dia_semana, dia_fin_semana,
      all_of(composicion_hogar),
      all_of(proveedores_externos),
      all_of(sociodemograficas),
      all_of(laborales),
      bs1:bs19,
      all_of(ingresos),
      all_of(acts_corregidas),
      all_of(acts_care_alternatives),
      all_of(tact_vars)
    ) %>%
    mutate(t_total = dplyr::select(., all_of(acts_corregidas)) %>% rowSums(na.rm = TRUE))

  data25[, acts_corregidas] <- round(data25[, acts_corregidas], 2)
  data25[, acts_care_alternatives] <- round(data25[, acts_care_alternatives], 2)
  data25[, "temp"] <- rowSums(data25[, acts_corregidas])
  data25[, "t_cpag_dormir"] <- data25[, "t_cpag_dormir"] - (data25[, "temp"] - 168)
  data25[, "temp"] <- rowSums(data25[, acts_corregidas])
  data25 <- data25 %>% dplyr::select(-c("temp"))

  data11 <- data_post %>%
    dplyr::select(
      all_of(identificadores),
      all_of(tipo_muestra),
      dia_semana, dia_fin_semana,
      all_of(composicion_hogar),
      all_of(proveedores_externos),
      all_of(sociodemograficas),
      all_of(laborales),
      bs1:bs19,
      all_of(ingresos),
      all_of(t_agregados),
      all_of(t_agregados_new),
      all_of(acts_care_alternatives)
    ) %>%
    mutate(t_total = dplyr::select(., all_of(t_agregados)) %>% rowSums(na.rm = TRUE))

  data11[, c(t_agregados, t_agregados_new)] <- round(data11[, c(t_agregados, t_agregados_new)], 2)
  data11[, acts_care_alternatives] <- round(data11[, acts_care_alternatives], 2)
  # Sleep absorbs the rounding residual of each classification separately.
  data11[, "t_sleep"] <- data11[, "t_sleep"] - (rowSums(data11[, t_agregados]) - 168)
  data11[, "Tc_sleep"] <- data11[, "Tc_sleep"] - (rowSums(data11[, t_agregados_new]) - 168)
  data11[, "t_total"] <- rowSums(data11[, t_agregados])
  data11 <- data11 %>% dplyr::select(-any_of("temp"))

  check_168 <- function(df, cols, label) {
    gap <- max(abs(rowSums(df[, cols]) - 168))
    if (gap > 1e-6) stop(label, " does not add up to 168 hours (max gap ", gap, ")")
  }
  check_168(data25, acts_corregidas, "enut-ii-raw activities")
  check_168(data11, t_agregados, "enut-ii aggregated classification")
  check_168(data11, t_agregados_new, "enut-ii Tw/Tf/Tc classification")

  return(list(data25 = data25, data11 = data11))
}
###### MANTENER POR AHI ########
# grupos <- data %>%
#   group_by(dia_semana, años_escolaridad, trabaja, rango_edad,sexo) %>%
#   summarise(cuantos = n())
#######

# Hot-deck tipo Hawkins (2021, cap. 7) para la composicion del gasto. Cada hogar ENUT recibe el
# promedio de las participaciones de gasto (10 divisiones) de m hogares EPF sorteados de su celda,
# con m = integrantes de 15 anios o mas. Celdas, de la mas fina a la mas gruesa (se usa la primera
# con al menos `min_donantes` hogares EPF):
#   1. estrato EPF x quintil: el estrato sale de la comuna (cut) y el NSE del hogar ENUT
#      (data/epf_estratos_comunas.csv, Metodologia IX EPF Tabla 16);
#   2. region x NSE x quintil (hogares de comunas que no estan en la EPF);
#   3. macrozona x NSE x quintil;  4. macrozona x quintil.
# Supuesto: el NSE de la ENUT es la misma clasificacion de la UPM que usa la EPF (1 bajo, 3 alto).
# Con el mismo sorteo devuelve tambien las razones JDI: el promedio de (10 divisiones, savings) /
# ingreso del hogar de los mismos donantes, que suma 1 (ahorro y gasto salen del mismo donante).
imputar_hawkins <- function(data_hogar, gastos, min_donantes = 10, semilla = 2023) {
  set.seed(semilla)
  mapa <- read.csv("data/epf_estratos_comunas.csv")
  gastos$region <- mapa$region[match(gastos$estrato, mapa$estrato)]
  shares <- as.matrix(gastos[, paste0("g.", alts)]) / gastos$total_expenses
  colnames(shares) <- alts
  razones <- as.matrix(gastos[, c(paste0("g.", alts), "savings")]) / gastos$ingreso_hogar
  colnames(razones) <- c(alts, "savings")

  donantes <- function(cut, nse, region, macrozona, quintil) {
    estratos <- mapa$estrato[mapa$cut == cut & mapa$nse == nse]
    q <- gastos$quintil == quintil
    celdas <- list(
      "estrato x quintil" = q & gastos$estrato %in% estratos,
      "region x NSE x quintil" = q & gastos$region == region & gastos$nse == nse,
      "macrozona x NSE x quintil" = q & gastos$macrozona == macrozona & gastos$nse == nse,
      "macrozona x quintil" = q & gastos$macrozona == macrozona
    )
    for (nivel in names(celdas)) {
      idx <- which(celdas[[nivel]] %in% TRUE)
      if (length(idx) >= min_donantes) return(list(idx = idx, nivel = nivel))
    }
    list(idx = which(q), nivel = "quintil")
  }

  data_hogar <- data_hogar %>% mutate(region = cut %/% 1000, clave = paste(cut, NSE, quintil, macrozona))
  P <- matrix(NA_real_, nrow(data_hogar), length(alts), dimnames = list(NULL, alts))
  R <- matrix(NA_real_, nrow(data_hogar), ncol(razones), dimnames = list(NULL, colnames(razones)))
  nivel <- character(nrow(data_hogar))
  for (k in unique(data_hogar$clave)) {
    filas <- which(data_hogar$clave == k)
    h <- data_hogar[filas[1], ]
    d <- donantes(h$cut, h$NSE, h$region, h$macrozona, h$quintil)
    for (i in filas) {
      m <- max(1, data_hogar$n_15_mas[i])
      sorteo <- d$idx[sample.int(length(d$idx), m, replace = TRUE)]
      P[i, ] <- colMeans(shares[sorteo, , drop = FALSE])
      R[i, ] <- colMeans(razones[sorteo, , drop = FALSE])
    }
    nivel[filas] <- d$nivel
  }
  print(table(nivel))
  list(shares = P, razones = R)
}

# ---------------------------------------------------------------------------------------------------------
# Inferencia de distribucion conjunta (JDI), Ye et al. (2017, JASSS) y Hawkins y Habib (2022,
# Transportation). La ENUT y la EPF solo comparten covariables de hogar D (quintil, region, integrantes
# de 15+, trabajadores), asi que f(D, tiempo, gasto) = f_ENUT(D, tiempo) * g_EPF(gasto | D): imputar es
# sortear hogares EPF de g(gasto | D). El IPF deja la composicion de D de los donantes igual a la de la
# ENUT, y las celdas ralas se colapsan por la dimension de asociacion mas debil (el quintil nunca).
# Detalle en CHANGES.md, seccion 10.
# ---------------------------------------------------------------------------------------------------------

# Dimensiones comunes D de una tabla de hogares (ENUT o EPF), ya agrupadas.
dimensiones_jdi <- function(d) {
  data.frame(
    quintil = d$quintil,
    region = d$region,
    n15 = pmin(pmax(d$n_15_mas, 1), 4),
    ntrab = pmin(d$n_trabajadores, 3)
  )
}

# Chi-cuadrado de independencia sobre la tabla ponderada. Los pesos se normalizan a suma n (muestra
# efectiva = observaciones), si no el estadistico crece con el factor de expansion. Devuelve el
# estadistico, los grados de libertad, el valor p y la V de Cramer.
chi2_independencia <- function(x, y, w = NULL) {
  if (is.null(w)) w <- rep(1, length(x))
  ok <- !is.na(x) & !is.na(y) & !is.na(w)
  x <- x[ok]; y <- y[ok]; w <- w[ok]
  n <- length(x)
  w <- w * n / sum(w)
  tab <- tapply(w, list(x, y), sum)
  tab[is.na(tab)] <- 0
  tab <- tab[rowSums(tab) > 0, colSums(tab) > 0, drop = FALSE]
  if (nrow(tab) < 2 || ncol(tab) < 2) return(list(estadistico = 0, gl = 0, p = 1, V = 0))
  esperado <- outer(rowSums(tab), colSums(tab)) / sum(tab)
  est <- sum((tab - esperado)^2 / esperado)
  gl <- (nrow(tab) - 1) * (ncol(tab) - 1)
  list(
    estadistico = est, gl = gl, p = pchisq(est, gl, lower.tail = FALSE),
    V = sqrt(est / (n * min(nrow(tab) - 1, ncol(tab) - 1)))
  )
}

# Pruebas de cada dimension contra cada clase de gasto (columnas de `epf`).
pruebas_independencia <- function(epf, dims, clases, w, alpha = 0.05) {
  filas <- lapply(dims, function(dim) {
    do.call(rbind, lapply(clases, function(cl) {
      t <- chi2_independencia(epf[[dim]], epf[[cl]], w)
      data.frame(
        dimension = dim, clase = cl, estadistico = t$estadistico, gl = t$gl, p = t$p, V = t$V,
        asociada = t$p < alpha
      )
    }))
  })
  do.call(rbind, filas)
}

# Una dimension independiente de las tres clases sale de D (Ye eq. 9). Las de `fijas` (el quintil) se
# quedan siempre. V = mayor V de Cramer entre las clases, por dimension retenida.
dimensiones_retenidas <- function(tabla, dims, fijas = "quintil") {
  retenidas <- dims[vapply(dims, function(d) any(tabla$asociada[tabla$dimension == d]) || d %in% fijas, logical(1))]
  V <- vapply(retenidas, function(d) max(tabla$V[tabla$dimension == d]), numeric(1))
  list(retenidas = retenidas, V = V)
}

# Quintiles ponderados de x (los empates comparten clase).
cuantiles_ponderados <- function(x, w, k = 5) {
  ux <- sort(unique(x))
  g <- match(x, ux)
  acum <- cumsum(as.vector(rowsum(w, g))) / sum(w)
  pmin(ceiling(acum[g] * k - 1e-12), k)
}

# IPF multivariado (Ye 3.9-3.10): ajusta los pesos de los hogares EPF (`perfiles`, una fila por hogar,
# dimensiones en columnas) hasta que las marginales de una y de dos dimensiones coincidan con las del
# objetivo (ENUT, ponderado por `w_objetivo`), ambas como participaciones. Las celdas vacias en la EPF
# siguen vacias. Los hogares del objetivo con un perfil sin hogares EPF en alguna marginal se sacan antes
# de calcular las metas (masa_sin_soporte: su participacion en el objetivo); si no, las marginales
# renormalizadas por separado serian inconsistentes entre si y el IPF no podria converger. Una celda EPF
# sin objetivo queda en cero.
ipf_multivariado <- function(perfiles, w, objetivo, w_objetivo = NULL, tol = 1e-8, max_iter = 1000,
                             error_si_falla = TRUE) {
  dims <- names(perfiles)
  if (is.null(w_objetivo)) w_objetivo <- rep(1, nrow(objetivo))
  clave <- function(d, v) do.call(paste, c(lapply(d[v], as.character), sep = "|"))
  margenes <- c(as.list(dims), if (length(dims) >= 2) combn(dims, 2, simplify = FALSE))
  w <- w / sum(w)
  con_soporte <- Reduce(`&`, lapply(margenes, function(m) clave(objetivo, m) %in% unique(clave(perfiles, m)[w > 0])))
  masa_sin_soporte <- sum(w_objetivo[!con_soporte]) / sum(w_objetivo)
  objetivo <- objetivo[con_soporte, , drop = FALSE]
  w_objetivo <- w_objetivo[con_soporte]
  M <- lapply(margenes, function(m) {
    k_epf <- clave(perfiles, m)
    meta <- tapply(w_objetivo, clave(objetivo, m), sum)
    meta <- meta / sum(meta)
    list(k = match(k_epf, names(meta)), meta = as.vector(meta))
  })
  suma_celdas <- function(w, m) {
    ok <- !is.na(m$k)
    cur <- numeric(length(m$meta))
    s <- rowsum(w[ok], m$k[ok])
    cur[as.integer(rownames(s))] <- s[, 1]
    cur
  }
  error_max <- function(w) max(vapply(M, function(m) max(abs(suma_celdas(w, m) - m$meta)), numeric(1)))
  iter <- 0
  err <- error_max(w)
  while (err >= tol && iter < max_iter) {
    iter <- iter + 1
    for (m in M) {
      f <- m$meta / suma_celdas(w, m)
      f[!is.finite(f)] <- 0
      ok <- !is.na(m$k)
      w[ok] <- w[ok] * f[m$k[ok]]
      w[!ok] <- 0
    }
    w <- w / sum(w)
    err <- error_max(w)
  }
  convergio <- err < tol
  if (!convergio && error_si_falla) {
    stop(sprintf("IPF no convergio en %d iteraciones (error maximo %.3e)", max_iter, err))
  }
  list(w = w, iter = iter, error = err, convergio = convergio, masa_sin_soporte = masa_sin_soporte)
}

# Sorteo por integrante con colapso. Para cada hogar ENUT (fila de `enut`, con las columnas de
# `perfiles_epf`) sortea m hogares EPF con reemplazo, con probabilidad proporcional a `w` entre los
# hogares con exactamente su perfil. Con menos de `min_donantes` hogares saca la dimension de menor V
# (V: vector con nombre sobre las dimensiones colapsables; el quintil no esta y nunca se saca). Devuelve,
# por cada matriz de `matrices` (filas = hogares EPF), el promedio de los m donantes, y el nivel usado.
sorteo_jdi <- function(enut, perfiles_epf, w, V, m, matrices, min_donantes = 10, semilla = 2023) {
  if (!is.null(semilla)) set.seed(semilla)
  colapsables <- names(V)
  orden <- colapsables[order(V)]
  fijas <- setdiff(names(perfiles_epf), colapsables)
  niveles <- lapply(0:length(orden), function(j) setdiff(c(fijas, colapsables), orden[seq_len(j)]))
  etiqueta <- vapply(niveles, paste, character(1), collapse = "+")
  clave <- function(d, v) do.call(paste, c(lapply(d[v], as.character), sep = "|"))
  k_epf <- lapply(niveles, function(v) clave(perfiles_epf, v))
  k_enut <- lapply(niveles, function(v) clave(enut, v))
  positivo <- w > 0
  n <- nrow(enut)
  m <- rep_len(m, n)
  salida <- lapply(matrices, function(M) matrix(NA_real_, n, ncol(M), dimnames = list(NULL, colnames(M))))
  nivel <- character(n)
  cache <- new.env()
  for (i in seq_len(n)) {
    k0 <- k_enut[[1]][i]
    d <- cache[[k0]]
    if (is.null(d)) {
      for (j in seq_along(niveles)) {
        idx <- which(k_epf[[j]] == k_enut[[j]][i] & positivo)
        if (length(idx) >= min_donantes || j == length(niveles)) break
      }
      d <- list(idx = idx, nivel = etiqueta[j])
      cache[[k0]] <- d
    }
    if (length(d$idx) == 0) stop("sorteo_jdi: no hay donantes con el quintil del hogar ", i)
    s <- d$idx[sample.int(length(d$idx), m[i], replace = TRUE, prob = w[d$idx])]
    for (nm in names(matrices)) salida[[nm]][i, ] <- colMeans(matrices[[nm]][s, , drop = FALSE])
    nivel[i] <- d$nivel
  }
  c(salida, list(nivel = nivel))
}

# Imputacion de gasto por JDI. Devuelve lo mismo que imputar_hawkins (shares: participaciones de las
# 10 divisiones; razones: 10 divisiones y ahorro sobre el ingreso del hogar) mas `nivel` por hogar, la
# tabla chi-cuadrado y el resumen del IPF. `data_hogar` necesita quintil, region (o cut), n_15_mas,
# n_trabajadores y, si existe, fe_hogar (factor de expansion ENUT; sin el, ponderacion uniforme).
imputar_jdi <- function(data_hogar, gastos, min_donantes = 10, semilla = 2023, alpha = 0.05) {
  stopifnot(all(c("fe", "n_15_mas", "n_trabajadores", "region", "quintil") %in% names(gastos)))
  if (!"region" %in% names(data_hogar)) data_hogar$region <- data_hogar$cut %/% 1000
  w_enut <- if ("fe_hogar" %in% names(data_hogar)) data_hogar$fe_hogar else rep(1, nrow(data_hogar))
  stopifnot(!anyNA(w_enut))
  div <- paste0("g.", alts)
  shares <- as.matrix(gastos[, div]) / gastos$total_expenses
  colnames(shares) <- alts
  razones <- as.matrix(gastos[, c(div, "savings")]) / gastos$ingreso_hogar
  colnames(razones) <- c(alts, "savings")

  perf_epf <- dimensiones_jdi(gastos)
  perf_enut <- dimensiones_jdi(data_hogar)
  stopifnot(!anyNA(perf_enut), !anyNA(perf_epf))
  dims <- names(perf_epf)

  # clases de gasto: quintiles de ahorro/ingreso y de las participaciones Ec y Ef1
  ef1 <- rowSums(gastos[, paste0("g.", c("recreacion", "restaurantes", "comunicaciones"))])
  ec <- rowSums(gastos[, paste0("g.", c("cuentas", "hogar", "salud", "transporte", "educacion"))])
  epf <- cbind(perf_epf, data.frame(
    c_ahorro = cuantiles_ponderados(gastos$savings / gastos$ingreso_hogar, gastos$fe),
    c_ec = cuantiles_ponderados(ec / gastos$total_expenses, gastos$fe),
    c_ef1 = cuantiles_ponderados(ef1 / gastos$total_expenses, gastos$fe)
  ))
  pruebas <- pruebas_independencia(epf, dims, c("c_ahorro", "c_ec", "c_ef1"), gastos$fe, alpha)
  sel <- dimensiones_retenidas(pruebas, dims, fijas = "quintil")
  cat("\nJDI: chi-cuadrado dimension x clase de gasto (EPF ponderada, alpha =", alpha, ")\n")
  print(transform(pruebas, estadistico = round(estadistico, 1), p = signif(p, 3), V = round(V, 3)), row.names = FALSE)
  cat("Dimensiones retenidas:", paste(sel$retenidas, collapse = ", "), "\n")
  retenidas <- sel$retenidas

  ipf <- ipf_multivariado(perf_epf[, retenidas, drop = FALSE], gastos$fe, perf_enut[, retenidas, drop = FALSE],
    w_objetivo = w_enut
  )
  cat(sprintf(
    "JDI IPF: convergio en %d iteraciones, error maximo de marginales %.2e, masa ENUT sin soporte EPF %.4f\n",
    ipf$iter, ipf$error, ipf$masa_sin_soporte
  ))

  V <- sel$V[setdiff(names(sel$V), "quintil")]
  s <- sorteo_jdi(perf_enut[, retenidas, drop = FALSE], perf_epf[, retenidas, drop = FALSE], ipf$w, V,
    pmax(1, data_hogar$n_15_mas), list(shares = shares, razones = razones), min_donantes, semilla
  )
  cat("JDI: hogares por nivel de colapso\n")
  print(table(s$nivel))
  list(
    shares = s$shares, razones = s$razones, nivel = s$nivel, pruebas = pruebas,
    ipf = ipf[c("iter", "error", "masa_sin_soporte")]
  )
}

# Ef0 (alimentos), Ef1, Ef2 y Ec (las grandes categorias de utils.R) desde montos de las 10 divisiones.
# Ef1 incluye alcohol y tabaco porque ya estan plegados en recreacion (plegar_alcohol).
agregar_categorias <- function(M, sufijo) {
  out <- data.frame(
    M[, "alimentos"],
    rowSums(M[, c("recreacion", "restaurantes", "comunicaciones"), drop = FALSE]),
    M[, "vestimenta"],
    rowSums(M[, c("cuentas", "hogar", "salud", "transporte", "educacion"), drop = FALSE])
  )
  names(out) <- paste0(c("Ef0", "Ef1", "Ef2", "Ec"), sufijo)
  out
}

# metodo: "jdi" (inferencia de distribucion conjunta, por defecto), "hotdeck" (celdas Hawkins, seccion 8
# de CHANGES.md) o "fmnl". `pesos`: data.frame(id_hog, fe_hogar) con el factor de expansion ENUT del hogar
# (fe_ch); sin el, el JDI pondera uniforme los hogares ENUT.
imputacion_gastos <- function(data, metodo = "jdi", pesos = NULL) {
  metodo <- match.arg(metodo, c("jdi", "hotdeck", "fmnl"))
  library("minpack.lm")
  data_hogar <- data %>%
    dplyr::select(
      id_hog, n_personas, n_menores_0_4, n_menores_5_14, n_personas_15_65, n_menores_18, n_trabajadores, n_personas,
      edad_promedio, quintil, macrozona, ingreso_hogar, income_person_week, n_profesionales, cut, NSE, n_15_mas
    ) %>%
    distinct(id_hog, .keep_all = T) %>%
    mutate(
      n_personas_cut = case_when(n_personas >= 7 ~ 7, T ~ n_personas),
      n_menores_0_4_cut = case_when(n_menores_0_4 >= 3 ~ 3, T ~ n_menores_0_4),
      # same coding as the EPF data the FMNL and savings models were fitted on
      # (expenditures.R): 2 or more children aged 5 to 14 are coded as 3
      n_menores_5_14_cut = case_when(n_menores_5_14 >= 2 ~ 3, T ~ n_menores_5_14),
      n_personas_15_65_cut = case_when(n_personas_15_65 >= 3 ~ 3, T ~ n_personas_15_65),
      n_trabajadores_cut = case_when(n_trabajadores >= 3 ~ 3, T ~ n_trabajadores),
      n_profesionales_cut = case_when(n_profesionales >= 2 ~ 3, T ~ n_profesionales)
    )
  # hist(gastos$savings/gastos$ingresos, breaks =100)
  gastos <<- read.csv("data/gastos.csv") %>% filter(ingreso_hogar > 0, savings / ingreso_hogar > -1)

  model <<- apollo_loadModel("output/FMNL-epf-ix")
  lin_reg <<- lm(
    savings ~
      ingreso_hogar +
      (quintil == 2) + (quintil == 3) + (quintil == 4) + (quintil == 5) +
      n_menores_0_4_cut + n_menores_5_14_cut + n_personas_15_65_cut + n_personas_cut +
      n_trabajadores_cut + n_profesionales_cut +
      edad_promedio +
      (macrozona == "norte") + (macrozona == "centro") + (macrozona == "sur"),
    data = gastos
  )
  summary(lin_reg)
  codigos <<- c(
    g.alimentos = "01", g.vestimenta = "03", g.cuentas = "04", g.hogar = "05", g.salud = "06",
    g.transporte = "07", g.comunicaciones = "08", g.recreacion = "09", g.educacion = "10", g.restaurantes = "11"
  )
  alts <<- c(
    "alimentos", "vestimenta", "cuentas", "hogar", "salud", "transporte",
    "comunicaciones", "recreacion", "educacion", "restaurantes"
  )

  mask <- data_hogar[, "ingreso_hogar"] >= 0
  data_hogar[mask, "savings"] <- predict(lin_reg, data_hogar[mask, ])

  sum(mask)
  sum((data_hogar[mask, "savings"] < 0), na.rm = T)
  sum((data_hogar[mask, "savings"] > data_hogar[mask, "ingreso_hogar"]), na.rm = T)
  data_hogar %>%
    distinct(id_hog, .keep_all = T) %>%
    group_by(quintil) %>%
    summarise(
      mean_savings = mean(savings, na.rm = TRUE),
      median_savings = median(savings, na.rm = TRUE),
      median_income = median(ingreso_hogar),
      min_hogar = min(ingreso_hogar)
    )
  gastos %>%
    group_by(quintil) %>%
    summarise(
      mean_savings = mean(savings, na.rm = TRUE),
      median_savings = median(savings, na.rm = TRUE),
      median_income = median(ingreso_hogar),
      min_hogar = min(ingreso_hogar)
    )

  data_hogar <- data_hogar %>%
    mutate(savings = case_when(
      # savings < 0 ~ 0,
      savings > ingreso_hogar ~ ingreso_hogar, T ~ savings
    ))
  data_hogar$id_hogar <- 1:nrow(data_hogar)

  if (!is.null(pesos)) {
    data_hogar$fe_hogar <- pesos$fe_hogar[match(data_hogar$id_hog, pesos$id_hog)]
    stopifnot(!anyNA(data_hogar$fe_hogar))
  }
  hd <- if (metodo == "hotdeck") imputar_hawkins(data_hogar, gastos) else imputar_jdi(data_hogar, gastos)
  if (metodo == "jdi") {
    write.csv(hd$pruebas, "output/jdi_chi2.csv", row.names = FALSE)
    write.csv(data.frame(id_hog = data_hogar$id_hog, nivel = hd$nivel), "output/jdi_nivel.csv", row.names = FALSE)
  }
  # the FMNL uses region (from the comuna) and NSE; a household without NSE takes the base level 1
  data_hogar <- data_hogar %>% mutate(region = cut %/% 1000, nse = ifelse(is.na(NSE), 1, NSE))
  apollo_initialise()
  apollo_control <<- list(
    modelName       = "FMNL",
    modelDescr      = "Fractional MNL model on time use data",
    indivID         = "id_hogar",
    outputDirectory = "output"
  )
  apollo_probabilities <<- model$apollo_probabilities
  database <<- data_hogar
  database[, names(codigos)] <<- 0
  apollo_beta <<- model$apollo_beta
  apollo_fixed <<- model$apollo_fixed
  apollo_inputs <<- apollo_validateInputs()
  predictions_base <<- apollo_prediction(model, apollo_probabilities, apollo_inputs)
  gasto_hogar <- data_hogar$ingreso_hogar - data_hogar$savings
  fmnl <- as.matrix(predictions_base[, alts]) * gasto_hogar
  # metodo elige las 10 divisiones sin sufijo: JDI o hot-deck (participaciones), o FMNL, con el ahorro
  # de la regresion. Las columnas _fmnl y _jdi se guardan siempre (ver CHANGES.md).
  data_hogar[, alts] <- if (metodo != "fmnl") hd$shares * gasto_hogar else fmnl
  jdi <- hd$razones * data_hogar$ingreso_hogar
  stopifnot(isTRUE(all.equal(rowSums(jdi), data_hogar$ingreso_hogar, check.attributes = FALSE)))
  sufijos <- c(
    paste0(c("Ef0", "Ef1", "Ef2", "Ec", "savings"), "_jdi"),
    paste0(c("Ef0", "Ef1", "Ef2", "Ec", "savings"), "_fmnl")
  )
  data_hogar <- cbind(
    data_hogar,
    agregar_categorias(jdi, "_jdi"), savings_jdi = jdi[, "savings"],
    agregar_categorias(fmnl, "_fmnl"), savings_fmnl = data_hogar$savings
  )

  data <- merge(data, data_hogar[, c("id_hog", "savings", alts, sufijos)], by = "id_hog", all.x = TRUE)
  data[, c("savings", alts, sufijos)] <- sweep(data[, c("savings", alts, sufijos)], 1, unlist(data[, "prop_ing_hogar"]), "*")
  data[, "total_expenses"] <- data[, "ing_personal"] - data["savings"]

  data[, c(alts, sufijos)] <- round(data[, c(alts, sufijos)], 2)

  # ingresos     <- c("ing_ocuppal", "ing_trab", "ing_jub_aps", "ing_g", "ing_mon", "ing_mon_pc", "ing_gpp", "ing_personal", "income_person_week")
  # data[, ingresos] = data[, ingresos] * 1000
  # data[, alts] = data[, alts] * 1000
  # data[, "savings"] = data[, "savings"] * 1000
  # data[, "w"] = data[, "w"] * 1000

  return(data)
}

# ---------------------------------------------------------------------------
# English renaming
# ---------------------------------------------------------------------------

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
  n_children_0_4       = "n_menores_0_4",
  n_children_5_14      = "n_menores_5_14",
  n_children_0_14      = "n_nna",
  n_adults_15_65       = "n_personas_15_65",
  n_underage           = "n_menores_18",
  n_adults             = "n_mayores",
  n_time_reporters     = "n_tiempo",
  n_workers            = "n_trabajadores",
  n_professionals      = "n_profesionales",
  n_elderly            = "n_tercera_edad",
  has_elderly          = "hay_tercera_edad",
  household_size       = "n_personas",
  n_members_15_plus    = "n_15_mas",
  comuna_code          = "cut",
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
  teleworks            = "teletrabaja",
  work_schedule        = "jornada_laboral",
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

rename_to_english_raw <- function(data) {
  time_mapping <- c(
    t_paid_work              = "t_to",
    t_job_search             = "t_to_js",
    t_care_essential         = "t_tcnr_ce",
    t_care_education_related = "t_tcnr_re",
    t_care_other             = "t_tcnr_oac",
    # alternative care decomposition
    t_care_0_4               = "t_tcnr_0_4",
    t_care_5_14              = "t_tcnr_5_14",
    t_care_children_teens    = "t_tcnr_nna",
    t_care_15_65             = "t_tcnr_15_65",
    t_care_66_plus           = "t_tcnr_66",
    t_care_disability        = "t_tcnr_psdf",
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
    t_leisure_events         = "t_vsyo_ev",
    t_leisure_sports         = "t_vsyo_dep",
    t_rest                   = "t_descanso",
    t_media_reading          = "t_mcm_leer",
    t_media_audio            = "t_mcm_audio",
    t_media_video            = "t_mcm_video",
    t_media_computer         = "t_mcm_computador",
    t_commute_to               = "t_tto",
    t_commute_ed               = "t_ted",
    t_commute_cpaf_cp          = "t_tcpaf_cp",
    t_commute_tdnr_admnhog     = "t_ttdnr_admnhog",
    t_commute_tdnr_comphog     = "t_ttdnr_comphog",
    t_commute_tcnr_re          = "t_ttcnr_re",
    t_commute_tcnr_ce          = "t_ttcnr_oac_health",
    t_commute_tcnr_oac         = "t_ttcnr_oac_work"
  )
  swb_mapping <- setNames(paste0("bs", 1:19), paste0("swb", 1:19))
  data %>% dplyr::rename(any_of(c(.rename_common_eng, time_mapping, swb_mapping)))
}

rename_to_english_enut <- function(data) {
  swb_mapping <- setNames(paste0("bs", 1:19), paste0("swb", 1:19))
  data %>% dplyr::rename(any_of(c(.rename_common_eng, swb_mapping)))
}
