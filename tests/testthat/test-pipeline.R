test_that("Pipelines work", {
    pipeline <- c(
        adder = llama3() |> instruct("Return a single number"),
        namer = llama3() |> instruct("Return only the name of the number")
    )
    ans <- predict(pipeline, "1 + 1")
    expect_identical(ans, "two")
})
