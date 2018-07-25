context("Checking Value Types")

source("Load.R")

test_that("FLE is Numeric", {
  a <- get_enrolment("FLE", username = uname, password = pwd)
  expect_type(a$FLE, "double")
})

test_that("Headcount is Numeric", {
  a <- get_enrolment("Unique Student Static", username = uname, password = pwd)
  expect_type(a$`Unique Student Static`, "double")
})
