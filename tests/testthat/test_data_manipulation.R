library(testthat)
library(mascutils)

context("data manipulation functions")

load("D.Rda")

test_that("go_first",{
  d <- data_frame(x = 1:2, y = c(NA,NA), z = c(NA,4))
  expect_equal(colnames(go_first(d, ~z))[1], "z")
})

test_that("discard_all_na",{
  d <- data_frame(x = 1:2, y = c(NA,NA), z = c(NA,4))
  expect_equal(colnames(discard_all_na(d)), c("x", "z"))
})

