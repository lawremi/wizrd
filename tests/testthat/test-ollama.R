test_that("ollama chatting works", {
    model <- wizrd:::ollama_llama(server)
    ans <- predict(model, "Who created R?")
    expect_match(ans, "Robert.*Ross|Ross.*Robert")
})

test_that("we fail when the model does not exist", {
    expect_error(ollama_model("madeupmodel:1000b", pull = FALSE))
})
