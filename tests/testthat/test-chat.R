test_that("chat() and predict() work for text messages", {
    model <- llama(server)
    model@instructions <- "Respond with a single sentence"

    chat <- chat(model, "Who created R?")
    expect_length(chat@messages, 3L)
    expect_match(last_output(chat), "Robert.*Ross|Ross.*Robert")

    chat2 <- chat(chat, "When did they do it?")
    expect_length(chat2@messages, 5L)
    expect_match(last_output(chat2), "1993")

    expect_match(predict(model, "Who created the R language?"),
                 "Robert.*Ross|Ross.*Robert")
    expect_match(predict(chat, "When did they do it?"), "1993")
})

test_that("chat() and predict() work for images", {
    model <- llama_vision(server)

    temp_png <- tempfile(fileext = ".png")
    png(temp_png, type = "Xlib")
    plot(1:10, main = "Example Plot")
    raster <- as.raster(dev.capture())
    native_raster <- dev.capture(native = TRUE)
    dev.off()
    unlink(temp_png)

    model@instructions <- "When you receive an image, describe it in words"

    chat <- chat(model, raster)
    expect_match(last_output(chat), "scatter.*plot")
    msgs <- list(raster,
                 paste("Is there positive correlation in this plot?",
                       "Answer yes or no."))
    chat <- chat(model, msgs) # as a multipart message
    expect_match(tolower(last_output(chat)), "yes")
})

test_that("chat() can stream responses", {
    skip_on_cran() # avoid ollama dependency since we cannot cache
    is_mocking <- !is.null(getOption("httr2_mock"))
    if (is_mocking) { # cannot cache req_perform_connection()
        httptest2::stop_mocking()
        on.exit(httptest2::use_mock_api())
    }

    model <- llama(server)

    model@instructions <- "Respond with a single sentence"

    all_content <- ""
    stream_callback <- function(content) {
        all_content <<- paste0(all_content, content)
        TRUE
    }

    prompt <- "Who created R?"
    ans <- predict(model, prompt, stream_callback = stream_callback)
    expect_equal(ans, all_content)
})
