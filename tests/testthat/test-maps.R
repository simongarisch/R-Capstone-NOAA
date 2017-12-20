library(rcap)
context("Testing the creation of our leaflet maps and related functions")

test_that("eq_map runs without error", {
  map <-load_data() %>%
    eq_clean_data() %>%
    dplyr::filter(COUNTRY == "MEXICO" & lubridate::year(date) >= 2000) %>%
    eq_map(annot_col = "date")

  expect_is(map, "leaflet")
  expect_is(map, "htmlwidget")
})

test_that("eq_create_label runs without error", {
  locations <- c("Loc1", "Loc2")
  magnitude <- c(4.2, 7.1)
  deaths <- c(0, 1)
  df = data.frame(CLEAN_LOCATION_NAME=locations,
                  EQ_PRIMARY=magnitude, DEATHS=deaths)
  lbls <- eq_create_label(df)
  expect_equal(lbls[1], "<b>Location: </b>Loc1</br><b>Magnitude: </b>4.2</br><b>Total Deaths: </b>0")
  expect_equal(lbls[2], "<b>Location: </b>Loc2</br><b>Magnitude: </b>7.1</br><b>Total Deaths: </b>1")
})
