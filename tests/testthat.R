#devtools::use_testthat()
#https://tgmstat.wordpress.com/2013/06/26/devtools-and-testthat-r-packages/
Sys.setenv("R_TESTS" = "")
library(testthat)
library(rcap)

test_check("rcap")
