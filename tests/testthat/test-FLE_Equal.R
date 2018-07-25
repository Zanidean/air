context("Checking FLE Summary")

source("Load.R")

test_that("Split FLE is equal to summarized FLE - Gender", {
  a <- get_enrolment("FLE", username = uname, password = pwd)
  b <- get_enrolment("FLE", c("Gender"), username = uname, password = pwd) %>%
    group_by(`Academic Year`) %>%
    summarise(FLE = sum(FLE))
  c <- (a$FLE - b$FLE) %>% sum
  expect_lt(c, 0.001)
})

test_that("Split FLE is equal to summarized FLE - Legal Status", {
  a <- get_enrolment("FLE", username = uname, password = pwd)
  b <- get_enrolment("FLE", c("Legal Status"),
                     username = uname, password = pwd) %>%
    group_by(`Academic Year`) %>%
    summarise(FLE = sum(FLE))
  c <- (a$FLE - b$FLE) %>% sum
  expect_lt(c, 0.001)
})

test_that("Split FLE is equal to summarized FLE - Provider", {
  a <- get_enrolment("FLE", username = uname, password = pwd)
  b <- get_enrolment("FLE", c("Provider"),
                     username = uname, password = pwd) %>%
    group_by(`Academic Year`) %>%
    summarise(FLE = sum(FLE))
  c <- (a$FLE - b$FLE) %>% sum
  expect_lt(c, 0.001)
})

