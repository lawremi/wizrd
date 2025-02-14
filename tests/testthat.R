library(testthat)
library(S7)
library(wizrd)

`:=` <- wizrd:::`:=`

setwd('tests')

httptest2::with_mock_dir("http", {
    test_check("wizrd")
})
