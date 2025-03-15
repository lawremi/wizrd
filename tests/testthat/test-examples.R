test_that("examples work", {
    model <- llama(server) |> instruct("No sentences") |>
        demonstrate("Who created R?", "Gentleman and Ihaka")
    ans <- predict(model, "Who created R?")
    testthat::expect_identical(ans, "Gentleman and Ihaka")
})
