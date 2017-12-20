library(rcap)
context("Testing that the clean_data function runs correctly")

test_that("Data gets cleaned correctly", {
  df <- load_data() %>% eq_clean_data()
  expect_is(df, "data.frame")        # the result will be a dataframe
  expect_is(df$date, "Date")         # the date column will be of the class date
  expect_is(df$LATITUDE, "numeric")  # that each of LATITUDE and LONGITUDE are numeric
  expect_is(df$LONGITUDE, "numeric")
})
