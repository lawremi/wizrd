library(testthat)
library(S7)
library(wizrd)

`:=` <- wizrd:::`:=`

results <- httptest2::with_mock_dir("http", {
    test_check("wizrd", reporter = "summary")
})
