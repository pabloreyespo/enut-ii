rm(list = ls()) # .rs.restartR()
pkgs <- c("ggplot2", "dplyr", "tidyr", "readr", "purrr", "tibble", "stringr", "forcats", "lubridate", "haven", "reshape2", "apollo")
invisible(lapply(pkgs, library, character.only = TRUE))

codigos <- c(
  g.alimentos = "01", g.vestimenta = "03", g.cuentas = "04", g.hogar = "05", g.salud = "06",
  g.transporte = "07", g.comunicaciones = "08", g.recreacion = "09", g.educacion = "10", g.restaurantes = "11"
)

vector_gastos <- c(
  "g.alimentos", "g.vestimenta", "g.cuentas", "g.hogar",
  "g.salud", "g.transporte", "g.comunicaciones", "g.recreacion", "g.educacion",
  "g.restaurantes", "total_expenses"
)

data_ingresos <- haven::read_dta("data/raw/base-personas-ix-epf-stata.dta")
data_gastos <- haven::read_dta("data/raw/base-gastos-ix-epf-stata.dta")

######################################################################################################
######################                      INGRESOS                          ########################
######################################################################################################

# codigos especialeS: -66, -77, (-99, missing)
data_ingresos <- data_ingresos %>% # sprincipal: jefe de hogar
  dplyr::select(
    folio,
    fe,
    macrozona,
    estrato_muestreo, # sampling stratum: group of comunas x socioeconomic level (Metodologia IX EPF, Tabla 16)
    cse, # socioeconomic level of the household (1 low, 2 middle, 3 high)
    npersonas,
    sprincipal,
    parentesco,
    sexo,
    edad,
    ecivil,
    ecompras,
    edue,
    eduactual,
    educurso,
    edunivel,
    edutermina,
    edudependencia,
    cae,
    caeg,
    ing_total_hogar_hd,
    salud,
    contribuciones,
    prevision,
    impuesto,
    ot05,
    ot06,
    gastot_hd,
    gastot_hd_ai,
    ing_disp_td,
    ing_disp_ti,
    # ing_disp_asal_hd,
    # ing_disp_hon_hd,
    # ing_neto_empleador_hd,
    # ing_disp_cpneg_hd,
    # falta INGDNP_HD
    # ing_disp_prof_hd,
    # ing_disp_asal_hd, #INGDOTA
    # ing_disp_jub_hd,
    # ing_disp_ot_ind,
    # ing_propiedad,
    # ing_financieros,
    # transferencias_emitidas,
    # transferencias_recibidas,
    ing_disp_hog_hd,
    ing_disp_hd
  ) %>%
  # mutate(ing_disp_prof_hd = case_when(ing_disp_prof_hd < 0 ~ 0, T ~ ing_disp_prof_hd)) %>%
  mutate(
    ot05 = case_when(ot05 < 0 ~ 0, T ~ ot05), ot06 = case_when(ot06 < 0 ~ 0, T ~ ot06),
    edad = case_when(edad < 0 ~ 0, T ~ edad)
  ) %>%
  mutate(parentesco = case_when(parentesco == 1 ~ 1, T ~ 0)) %>%
  filter_all(all_vars(. != -99)) %>%
  filter_all(all_vars(. != -88)) %>%
  arrange(ing_disp_hog_hd) %>%
  mutate(percentil = cumsum(fe) / sum(fe)) %>%
  mutate(quintil = case_when(
    percentil <= 0.20000001 ~ 1,
    percentil <= 0.40000001 ~ 2,
    percentil <= 0.60000001 ~ 3,
    percentil <= 0.80000001 ~ 4,
    T ~ 5
  )) %>%
  dplyr::select(-c(percentil, fe)) %>%
  group_by(folio) %>%
  summarise(
    n_menores_0_4 = sum(edad <= 4),
    n_personas = max(npersonas),
    macrozona = max(macrozona),
    n_menores_5_14 = sum(edad >= 5 & edad <= 14),
    n_personas_15_65 = sum(edad >= 15 & edad <= 65),
    n_trabajadores = sum((ing_disp_td + ing_disp_ti) > 0),
    income_person_week = mean(ing_disp_hog_hd) / n(),
    edad_promedio = mean(edad),
    # technical tertiary, professional or postgraduate degree completed (edunivel 10 to 14 in the IX EPF),
    # the same people the ENUT counts as nivel_escolaridad >= 4. The former edunivel >= 14 cut came from
    # the VIII EPF coding (14 = professional); in the IX EPF 14 is only a doctorate.
    n_profesionales = sum(edunivel >= 10 & edutermina != 2),
    ingreso_hogar = sum(ing_disp_hd),
    quintil = max(quintil),
    ing_disp_hog_hd = max(ing_disp_hog_hd),
    estrato = first(estrato_muestreo),
    nse = first(cse)
  ) %>%
  mutate(macrozona = case_when(
    macrozona == 1 ~ "norte",
    macrozona == 2 ~ "metropolitana",
    macrozona == 3 ~ "centro",
    macrozona == 4 ~ "sur"
  )) %>%
  ungroup() %>%
  distinct(folio, .keep_all = TRUE)


data_gastos <- data_gastos %>%
  filter(ccif != "04.2.1.01.01" & ccif != "04.2.2.01.01" & ccif != "04.2.2.01.02" & folio != "") %>%
  drop_na(folio) %>%
  mutate(codigo = d) %>% # paste(D,G, sep =".")) %>%
  mutate(codigo = case_when(
    d == "09" & g == "1" ~ "08",
    d == "02" ~ "01", # alcoho
    d == "12" & g == "1" ~ "06",
    d == "12" & g == "2" ~ "09",
    d == "12" & g == "3" ~ "03",
    d == "12" & g == "4" ~ "04",
    d == "12" & g == "5" ~ "04",
    d == "12" & g == "6" ~ "04",
    d == "12" & g == "7" ~ "04",
    d == "12" & g == "8" ~ "04",
    T ~ codigo
  )) %>%
  group_by(folio, codigo) %>%
  summarise(monto_total = sum(gasto)) %>%
  mutate(codigo = names(codigos)[match(codigo, codigos)]) %>%
  ungroup() %>%
  dcast(folio ~ codigo, value.var = "monto_total", fun.aggregate = sum) %>%
  dplyr::select(-c("NA")) %>%
  mutate_at(names(codigos), ~ replace_na(., 0))

data_ambos <- merge(data_ingresos, data_gastos, "folio") %>%
  arrange(folio) %>%
  mutate(total_expenses = dplyr::select(., names(codigos)) %>% rowSums()) %>% # legal.discounts
  mutate(savings = ingreso_hogar - total_expenses) %>%
  filter(total_expenses > 0 & ingreso_hogar > 0) # & EDAD > 0 & EDUE > 0 & EDUNIVEL>0

# data_ambos <- data_ambos %>% filter(JHOGAR == 1)
# data_ambos %>% group_by(quintil, macrozona) %>% summarise(minim = min(ing_disp_hog_hd)) %>% arrange(quintil, macrozona)
# data_ambos %>% group_by(quintil, macrozona) %>% summarise(minim = min(ingreso_hogar)) %>% arrange(quintil, macrozona)

gastos <- data_ambos %>%
  mutate(id_hogar = 1:n()) %>%
  dplyr::select(
    folio, id_hogar, all_of(vector_gastos), savings, n_personas,
    macrozona, quintil, estrato, nse, n_menores_0_4,
    n_menores_5_14, n_personas_15_65, n_trabajadores, edad_promedio,
    n_profesionales, income_person_week, ingreso_hogar
  )

gastos[, c(vector_gastos, "ingreso_hogar", "savings", "income_person_week")] <-
  gastos[, c(vector_gastos, "ingreso_hogar", "savings", "income_person_week")] / 4000 * 1.18 # inflación entre periodos de muestreo1.025 # (4 semanas, miles de pesos, 1.025 la inflación)

gastos <- gastos %>%
  mutate(
    n_personas_cut = case_when(n_personas >= 7 ~ 7, T ~ n_personas),
    n_menores_0_4_cut = case_when(n_menores_0_4 >= 3 ~ 3, T ~ n_menores_0_4),
    n_menores_5_14_cut = case_when(n_menores_5_14 >= 2 ~ 3, T ~ n_menores_5_14),
    n_personas_15_65_cut = case_when(n_personas_15_65 >= 3 ~ 3, T ~ n_personas_15_65),
    n_trabajadores_cut = case_when(n_trabajadores >= 3 ~ 3, T ~ n_trabajadores),
    n_profesionales_cut = case_when(n_profesionales >= 2 ~ 3, T ~ n_profesionales)
  )
# Location for the FMNL: region of the sampling stratum (data/epf_estratos_comunas.csv, Tabla 16 of
# the Metodologia IX EPF) and household NSE. Gran Santiago (region 13) and NSE 1 are the base levels.
mapa_estratos <- read.csv("data/epf_estratos_comunas.csv")
gastos$region <- mapa_estratos$region[match(gastos$estrato, mapa_estratos$estrato)]
write.csv(gastos, "data/gastos.csv")
# Rscript data_processing/expenditures.R --solo-datos stops here: the Hawkins hot-deck imputation
# (imputacion_gastos(metodo = "hawkins")) only needs data/gastos.csv, not the FMNL model.
if ("--solo-datos" %in% commandArgs(trailingOnly = TRUE)) quit(save = "no")

########################################################################################################################
################                                 MODELOS                                                ################
########################################################################################################################

alts <- c(
  "alimentos", "vestimenta", "cuentas", "hogar", "salud", "transporte",
  "comunicaciones", "recreacion", "educacion", "restaurantes"
)

temp <- sweep(gastos[, paste0("g.", alts)], 1, rowSums(gastos[, paste0("g.", alts)]), "/")
colMeans(temp)
apollo_initialise()
apollo_control <- list(
  modelName = "FMNL-epf-ix",
  modelDescr = "Fractional MNL model on time use data",
  indivID = "id_hogar",
  outputDirectory = "output",
  nCores = as.integer(Sys.getenv("APOLLO_CORES", "22")) # 22 on the cluster; lower it on a laptop
)

database <- gastos
database[, names(codigos)] <- database[, names(codigos)] / rowSums(database[, names(codigos)])

# Covariates of the FMNL. Location enters as region (Gran Santiago, region 13, is the base) and household
# NSE (1 is the base) instead of the former north / centre / south dummies: in a 5-fold cross validation on
# the EPF the RMSE of the Ef1 share drops from 0.160 to 0.158. Region and NSE are computed inside
# apollo_probabilities from the `region` and `nse` columns, so the saved model also predicts on the ENUT.
parametros <- c(
  "asc", "bnpersonas", "bnmenores_0_4", "bnmenores_5_14", "bnpersonas_15_65", "bntrabajadores",
  "bnprofesionales", "bedadpromedio", "bq2", "bq3", "bq4", "bq5",
  paste0("bregion", c(1:12, 14:16)), "bnse2", "bnse3"
)
apollo_beta <- setNames(rep(0, length(parametros) * length(alts)),
                        as.vector(outer(parametros, alts, paste, sep = "_")))
apollo_fixed <- paste(parametros, "vestimenta", sep = "_") # vestimenta is the base alternative

apollo_inputs <- apollo_validateInputs()

apollo_probabilities <- function(apollo_beta, apollo_inputs, functionality = "estimate") {
  apollo_attach(apollo_beta, apollo_inputs)
  on.exit(apollo_detach(apollo_beta, apollo_inputs))
  P <- list()
  V <- list()
  for (alt in c("alimentos", "vestimenta", "cuentas", "hogar", "salud", "transporte", "comunicaciones", "recreacion", "educacion", "restaurantes")) {
    V[[alt]] <- get(paste0("asc_", alt)) +
      get(paste0("bnpersonas_", alt)) * n_personas_cut +
      get(paste0("bnmenores_0_4_", alt)) * n_menores_0_4_cut +
      get(paste0("bnmenores_5_14_", alt)) * n_menores_5_14_cut +
      get(paste0("bnpersonas_15_65_", alt)) * n_personas_15_65_cut +
      get(paste0("bntrabajadores_", alt)) * n_trabajadores_cut +
      get(paste0("bnprofesionales_", alt)) * n_profesionales_cut +
      get(paste0("bedadpromedio_", alt)) * edad_promedio +
      get(paste0("bq2_", alt)) * (quintil == 2) +
      get(paste0("bq3_", alt)) * (quintil == 3) +
      get(paste0("bq4_", alt)) * (quintil == 4) +
      get(paste0("bq5_", alt)) * (quintil == 5) +
      get(paste0("bnse2_", alt)) * (nse == 2) +
      get(paste0("bnse3_", alt)) * (nse == 3)
    for (r in c(1:12, 14:16)) {
      V[[alt]] <- V[[alt]] + get(paste0("bregion", r, "_", alt)) * (region == r)
    }
  }

  ### Define settings for MNL models component
  fmnl_settings <- list(
    alternatives = c("alimentos", "vestimenta", "cuentas", "hogar", "salud", "transporte", "comunicaciones", "recreacion", "educacion", "restaurantes"),
    choiceShares = list(
      alimentos = g.alimentos, vestimenta = g.vestimenta, cuentas = g.cuentas,
      hogar = g.hogar, salud = g.salud, transporte = g.transporte, comunicaciones = g.comunicaciones,
      recreacion = g.recreacion, educacion = g.educacion, restaurantes = g.restaurantes
    ),
    utilities = V
  )

  P[["model"]] <- apollo_fmnl(fmnl_settings, functionality)
  P <- apollo_prepareProb(P, apollo_inputs, functionality)
  return(P)
}
model <- apollo_estimate(apollo_beta, apollo_fixed, apollo_probabilities, apollo_inputs)
apollo_modelOutput(model)
apollo_saveOutput(model)
