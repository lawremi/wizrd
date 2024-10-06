test_that("glue templating works", {
    model <- llama3()

    ans <- model |>
        accept_as("Output a {nchar}-letter word starting with '{initial}'.") |>
        predict(list(nchar = 4L, initial = "a"))
    expect_equal(ans, "Able")
})
