test_that("Pipelines work", {
    pipeline <- c(
        adder = llama3() |> instruct("Return a single number"),
        namer = llama3() |> instruct("Return only the name of the number")
    )
    ans <- predict(pipeline, "1 + 1")
    expect_identical(ans, "Two")

    pipeline$namer <- chat(pipeline$namer)
    cht <- chat(pipeline, "1 + 1")
    ans <- last_output(cht)
    expect_identical(ans, "Two")

    nested_ans <- last_output(cht@model$namer)
    expect_identical(ans, nested_ans)
})
