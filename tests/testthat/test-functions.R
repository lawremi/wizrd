skip_on_cran()

test_that("models can call functions", {
    model <- ollama_model("llama3.1", pull = TRUE)
}
