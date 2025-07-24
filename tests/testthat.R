library(testthat)
library(S7)
library(wizrd)

`:=` <- wizrd:::`:=`

server <- ollama_server()

if (interactive())
    setwd("tests")

dir <- file.path(getwd(), "testthat", "http")

results <- httptest2::with_mock_dir(dir, {
    test_check("wizrd", reporter = "summary")
})
