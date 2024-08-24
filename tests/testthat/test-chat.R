test_that("calling chat() and friends generates correct responses", {
    model <- stories260K()

##    options(wizrd.debug = 3L)
    expect_class(chat(model, "Tell me a story"), "Chat")
    expect_string(predict(model, "Tell me a story"))
})
