library(rcap)
context("Testing that the date column is created correctly from DAY, MONTH, YEAR")

test_that("The date column is created correctly", {
  years = c(1, 2017, 1985, 2001)
  months = c(12, 11, NA, NA)
  days = c(2, NA, 15, NA)

  dates <- get_date(days, months, years)
  expect_equal(length(dates), 4)             # we expect to get 4 dates back
  expect_is(dates, "Date")                   # of the class Date (as required by Coursera)
  expect_equal(format(dates[4], "%m"), "01") # NA months are replaced with Jan
  expect_equal(format(dates[4], "%d"), "01") # NA days are replaced with 1st
})
