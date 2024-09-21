test_that("chat() and predict() work for text messages", {
    model <- llama3()
    model@instructions <- "Respond with a single sentence"
    
    chat <- chat(model, "Who created R?")
    expect_length(chat@messages, 3L)
    expect_match(last_output(chat), "Robert.*Ross|Ross.*Robert")

    chat2 <- chat(chat, "When did they do it?")
    expect_length(chat2@messages, 5L)
    expect_match(last_output(chat2), "1992")
    
    expect_match(predict(model, "Who created R?"), "Robert.*Ross|Ross.*Robert")
    expect_match(predict(chat, "When did they do it?"), "1992")
})

test_that("chat() and predict() work for images", {
    model <- llava()

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
                 "Is there positive correlation in this plot? Answer yes or no.")
    chat <- chat(model, msgs) # as two separate messages
    expect_match(last_output(chat), "Yes")
    chat <- chat(model, list(msgs)) # as a single, multi-part message
    expect_match(last_output(chat), "Yes")
})

test_that("chat() can stream responses", {
    model <- llama3()
    
    model@instructions <- "Respond with a single sentence"

    all_content <- ""
    stream_callback <- function(content) {
        all_content <<- paste0(all_content, content)
        TRUE
    }
    
    chat <- chat(model, "Who created R?", stream_callback = stream_callback)
    
})

test_that("models can call R functions as tools", {
    model <- llama3()
    
    get_mean <- function(name) mean(get(name))
    model <- equip(model, get_mean, "Use to get the mean of an R variable")
    var <- 1:10
    model@instructions <-
        "Only respond with JSON, not in markdown. No sentences."
    output <- predict(model, "What is the mean of var?")
    expect_equal(jsonlite::fromJSON(output)$mean, mean(var))

    sig <- tool_signature(x = class_name)
    model <- model |> unequip("get_mean") |> equip(tool(mean, signature = sig))
    options(wizrd.debug = 0L)
    output <- predict(model, "What is the mean of var?")
    expect_equal(jsonlite::fromJSON(output)$mean, mean(var))

    model <- equip(model, mean)
    output <- predict(model, "What is the mean of `var`?")
    expect_equal(jsonlite::fromJSON(output)$mean, mean(var))
    
    sig <- tool_signature(class_data.frame, x = class_formula,
                          data = class_name, FUN = class_name,
                          subset = class_call)
    aggregate_tool <- tool(method(aggregate, class_formula), sig, "aggregate")
    model <- equip(model, aggregate_tool)
    model@instructions <- "Use the tools at your disposal. Return only numbers."
    output <- predict(model, "Mean of MPG.city for Audis in the Cars93 dataset")
    expect_equal(as.numeric(output), mean(var))
})
