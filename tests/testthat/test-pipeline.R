test_that("Pipelines work", {
    pipeline <- c(
        adder = test_model(server) |>
            instruct("Return only the number as digits. No words."),
        namer = test_model(server) |>
            instruct("Return only the English name of the number, title case. No punctuation.")
    )
    ans <- predict(pipeline, "1 + 1")
    expect_identical(ans, "Two")

    pipeline$namer <- chat(pipeline$namer)
    cht <- chat(pipeline, "1 + 1")
    ans <- last_output(cht)
    expect_identical(ans, "Two")

    nested_output <- last_output(cht@model$namer)
    expect_identical(ans, nested_output)
    nested_input <- wizrd:::last_input(cht@model$namer)
    expect_identical(nested_input, "2")
})
