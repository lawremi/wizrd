test_that("Pipelines work", {
    pipeline <- c(
        adder = llama(server) |> instruct("Return a single number"),
        namer = llama(server) |> instruct("Return only the name of the number")
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
