library(rcap)
context("Testing that the eq_location_clean function runs correctly")

test_that("Location gets cleaned correctly", {
  df <- load_data() %>% eq_location_clean
  expect_true(all(df$LOCATION_NAME[1:2] == c("JORDAN:  BAB-A-DARAA,AL-KARAK", "SYRIA:  UGARIT")))
  expect_true(all(df$CLEAN_LOCATION_NAME[1:2] == c("Bab-A-Daraa,Al-Karak", "Ugarit")))
})
