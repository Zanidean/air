context("Checking Returned Data Types")

source("Load.R")

uname <- "AZanidean"
pwd <- "668Marinr"

test_that("Returned Enrolment data is a tibble", {
  test_data <- get_enrolment("FLE", username = uname, password = pwd)
  expect_true(is.tibble(test_data))
})

test_that("Returned Applicant data is a tibble", {
  test_data <- get_applications("Unique Applicant Static",
                                username = uname, password = pwd)
  expect_true(is.tibble(test_data))
})
