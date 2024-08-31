test_that("chat() and predict() work for text messages", {
    model <- llama3()
    model@instructions <- "Respond with a single sentence"
    
    ##    options(wizrd.debug = 3L)
    chat <- chat(model, "Who created R?")
    expect_length(chat@messages, 3L)
    expect_match(last_output(chat), "Robert.*Ross|Ross.*Robert")

    chat2 <- chat(chat, "When did they do it?")
    expect_length(chat2@messages, 5L)
    expect_match(last_output(chat2), "1992")
    
    expect_match(predict(model, "Who created R?"), "Robert.*Ross|Ross.*Robert")
    expect_match(predict(chat, "When did they do it?"), "1992")
})
