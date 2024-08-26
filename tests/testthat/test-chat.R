test_that("calling chat() and friends generates correct responses", {
    model <- stories260K()

    ##    options(wizrd.debug = 3L)
    chat <- chat(model, "Tell me a story")
    expect_class(chat, "Chat")
    expect_length(chat@messages, 3L)
    chat <- chat(chat, "Tell me another story")
    expect_class(chat, "Chat")
    expect_length(chat@messages, 5L)
    
    expect_string(predict(model, "Tell me a story"))
})
