library(rcap)
context("Testing that the geoms run and return the correct objects")

#-----------------------------------------------------------------------------
#-----------------------------------------------------------------------------
test_that("geom_timeline runs correctly", {
  df <- df_earthquakes %>% dplyr::filter(COUNTRY %in% c("CHINA", "USA"), YEAR > 2000)
  plt <-   ggplot2::ggplot(df, ggplot2::aes(x = date, y = COUNTRY,
                          color = as.numeric(TOTAL_DEATHS),
                          size = as.numeric(EQ_PRIMARY),
                          label = CLEAN_LOCATION_NAME)) +
           geom_timeline() +
           ggplot2::labs(size = "Richter scale value", color = "# deaths") +
           theme(panel.background = element_blank(),
                 legend.position = "bottom",
                 axis.title.y = element_blank()) + xlab("DATE")
  expect_is(plt, "gg")
  expect_is(plt, "ggplot")
})

#-----------------------------------------------------------------------------
#-----------------------------------------------------------------------------
test_that("geom_timeline_label runs correctly", {
  df <- df_earthquakes %>% dplyr::filter(COUNTRY %in% c("CHINA", "USA"), YEAR > 2000)
  plt <-   ggplot2::ggplot(df, ggplot2::aes(x = date, y = COUNTRY,
                          color = as.numeric(TOTAL_DEATHS),
                          size = as.numeric(EQ_PRIMARY),
                          label = CLEAN_LOCATION_NAME)) +
    geom_timeline() +
    ggplot2::labs(size = "Richter scale value", color = "# deaths") +
    theme(panel.background = element_blank(),
          legend.position = "bottom",
          axis.title.y = element_blank()) + xlab("DATE") +
    geom_timeline_label(data=df)

  expect_is(plt, "gg")
  expect_is(plt, "ggplot")
})
#-----------------------------------------------------------------------------
#-----------------------------------------------------------------------------
