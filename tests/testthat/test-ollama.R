test_that("ollama chatting works", {
    model <- ollama_llama()
    ans <- predict(model, "Who created R?")
    expect_match(ans, "Robert.*Ross|Ross.*Robert")
})
