# Tests for the committed expenditure imputed by linear regression (Ec_lr).
# Run from enut-ii: Rscript tests/test_ec_lr.R
suppressPackageStartupMessages({library(dplyr); library(testthat)})
source("data_processing/processing_functions.R")

hogares <- function(n, semilla) {
  set.seed(semilla)
  d <- data.frame(
    ingreso_hogar = runif(n, 50, 900), quintil = sample(1:5, n, TRUE),
    n_menores_0_4_cut = sample(0:3, n, TRUE), n_menores_5_14_cut = sample(c(0, 1, 3), n, TRUE),
    n_personas_15_65_cut = sample(0:3, n, TRUE), n_personas_cut = sample(1:7, n, TRUE),
    n_trabajadores_cut = sample(0:3, n, TRUE), n_profesionales_cut = sample(c(0, 1, 3), n, TRUE),
    edad_promedio = runif(n, 20, 70), region = sample(1:16, n, TRUE), nse = sample(1:3, n, TRUE)
  )
  d$savings <- 0.1 * d$ingreso_hogar
  d
}

test_that("imputar_ec_lr recovers a linear committed expenditure and predicts it on new households", {
  epf <- hogares(4000, 1)
  ec <- 20 + 0.3 * epf$ingreso_hogar + 15 * (epf$quintil == 5) + 8 * epf$n_personas_cut +
    12 * (epf$region == 5) + 10 * (epf$nse == 3)
  partes <- c(0.4, 0.1, 0.2, 0.2, 0.1)
  for (k in seq_along(partes)) epf[[paste0("g.", c("cuentas", "hogar", "salud", "transporte", "educacion")[k])]] <- partes[k] * ec
  enut <- hogares(300, 2)
  enut$savings <- -enut$ingreso_hogar # the expenditure cap is far from every prediction
  r <- imputar_ec_lr(enut, epf)
  esperado <- 20 + 0.3 * enut$ingreso_hogar + 15 * (enut$quintil == 5) + 8 * enut$n_personas_cut +
    12 * (enut$region == 5) + 10 * (enut$nse == 3)
  expect_equal(unname(coef(r$modelo)["ingreso_hogar"]), 0.3, tolerance = 1e-8)
  expect_equal(r$ec, esperado, tolerance = 1e-8)
})

test_that("imputar_ec_lr keeps Ec between zero and the household expenditure", {
  epf <- hogares(2000, 3)
  ec <- 0.9 * epf$ingreso_hogar - 100
  for (g in c("cuentas", "hogar", "salud", "transporte", "educacion")) epf[[paste0("g.", g)]] <- ec / 5
  enut <- hogares(300, 4)
  enut$savings[1:50] <- 0.6 * enut$ingreso_hogar[1:50]
  r <- imputar_ec_lr(enut, epf)
  expect_true(all(r$ec >= 0))
  expect_true(all(r$ec <= enut$ingreso_hogar - enut$savings + 1e-12))
  expect_true(any(r$ec == 0) && any(r$ec == (enut$ingreso_hogar - enut$savings)))
})
