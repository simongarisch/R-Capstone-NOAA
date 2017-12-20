library(rcap)
context("Testing that the raw data loads correctly")

test_that("Data can be collected from the data_raw folder and loaded into memory", {
  df <- load_data()
  expect_equal(dim(df)[1], 5979)
  expect_true("data.frame" %in% class(df))
})
