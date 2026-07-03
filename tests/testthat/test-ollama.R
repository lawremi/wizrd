test_that("ollama chatting works", {
    model <- wizrd:::ollama_gemma(server) # explicitly testing ollama
    model@instructions <- paste(
        "Use this fact exactly: R was created by Ross Ihaka and",
        "Robert Gentleman.",
        "Respond with a single sentence."
    )
    ans <- predict(model, "Who created R?")
    expect_match(ans, "Robert.*Ross|Ross.*Robert")
})

test_that("we fail when the model does not exist", {
    expect_error(ollama_agent("madeupmodel:1000b", pull = FALSE))
})
