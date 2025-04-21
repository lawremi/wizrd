test_that("calling predict() generates correct responses", {
    model <- openai_agent("gpt-4o-mini", temperature = 0)
    model@instructions <- "Respond with a single sentence"

    msg <- predict(model, "Who created R?")
    expect_match(msg, "Robert.*Ross|Ross.*Robert")
})
