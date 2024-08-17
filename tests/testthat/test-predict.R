test_that("calling predict() generates responses", {
    model <- stories260K()

    expect_string(predict(model, "Where did R come from?"))
})
