context("Checking Returned Data Types")

library(testthat)
library(air)

test_that("Returned data is a tibble", {
  test_data <- get_enrolment("FLE")
  expect_true(is.tibble(test_data))
})
