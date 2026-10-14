# Tests for repartir_ingreso(). Run from enut-ii: Rscript tests/test_ingresos.R
suppressPackageStartupMessages({library(dplyr); library(tidyr); library(testthat)})
source("data_processing/processing_functions.R")

hogar <- function(...) {
  m <- rbind(...)
  tibble(id_hog = m[, 1], edad = m[, 2], ing_trab = m[, 3], ing_jub_aps = m[, 4], ing_otros = m[, 5], ing_t_hogar = m[, 6])
}

test_that("rents and transfers stay with whoever receives them", {
  d <- repartir_ingreso(hogar(c(1, 40, 500, 0, 0, 650), c(1, 38, 0, 0, 150, 0)), "id_hog", "edad")
  expect_equal(d$ing_gpp, c(0, 150))
  expect_equal(d$prop_ing_hogar, c(500, 150) / 650)
})

test_that("residual is split by own income", {
  d <- repartir_ingreso(hogar(c(1, 40, 300, 0, 0, 500), c(1, 20, 0, 0, 100, 0)), "id_hog", "edad")
  expect_equal(d$ing_gpp, c(75, 125))
  expect_equal(sum(d$ingreso_personal), 500)
})

test_that("household without own income is split among members aged 15+", {
  d <- repartir_ingreso(hogar(c(1, 50, 0, 0, 0, 90), c(1, 16, 0, 0, 0, 0), c(1, 10, 0, 0, 0, 0)), "id_hog", "edad")
  expect_equal(d$ingreso_personal, c(45, 45, 0))
  expect_equal(d$prop_ing_hogar, c(0.5, 0.5, 0))
  expect_equal(d$ingreso_hogar, c(90, 90, 90))
})

test_that("household without income stays at zero", {
  d <- repartir_ingreso(hogar(c(1, 50, 0, 0, 0, 0), c(1, 20, 0, 0, 0, 0)), "id_hog", "edad")
  expect_true(all(d$ingreso_personal == 0 & d$prop_ing_hogar == 0 & d$ingreso_hogar == 0))
})

test_that("household income is preserved and households do not mix", {
  d <- repartir_ingreso(hogar(c(1, 40, 500, 0, 0, 650), c(1, 38, 0, 0, 150, 0),
                              c(2, 70, 0, 200, 0, 260), c(2, 68, 0, 0, 0, 0)), "id_hog", "edad")
  expect_equal(as.vector(tapply(d$ingreso_personal, d$id_hog, sum)), c(650, 260))
  expect_equal(d$ing_gpp, c(0, 150, 60, 0))
  expect_equal(d$ingreso_hogar, c(650, 650, 260, 260))
})

test_that("ingreso_valido drops no sabe / no responde codes", {
  expect_equal(ingreso_valido(c(-88, -99, NA, 0, 150000)), c(0, 0, 0, 0, 150000))
})
