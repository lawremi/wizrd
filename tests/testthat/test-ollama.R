test_that("ollama chatting works", {
    model <- ollama_model("llama3.1:8b-instruct-q4_K_M", temperature = 0)
    ans <- predict(model, "Who created R?")
    expect_match(ans, "Robert.*Ross|Ross.*Robert")
})
