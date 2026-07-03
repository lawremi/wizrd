test_that("structured output works", {
    model <- test_model(server) |>
        instruct("Extract person names from the user text.") |>
        output_as(data.frame(first_name = character(), last_name = character()))
    r_creators <- "Text: R was created by Ross Ihaka and Robert Gentleman."
    ans <- predict(model, r_creators)
    expect_contains(ans$first_name, "Robert")
    expect_contains(ans$last_name, "Gentleman")

    Person <- S7::new_class("Person", properties = list(
        first_name = S7::class_character,
        last_name = S7::class_character
    ))
    model <- output_as(model, Person)
    ans <- predict(model, "Text: R was created by Robert Gentleman.")
    expect_contains(ans@first_name, "Robert")
    expect_contains(ans@last_name, "Gentleman")

    Person <- S7::new_class("Person", properties = list(
        first_name = eleven::scalar(class_character),
        last_name = eleven::scalar(class_character)
    ))
    model <- output_as(model, eleven::list_of(Person))
    ans <- predict(model, r_creators)
    expect_match(ans[[1L]]@first_name, "Robert|Ross")

    model <- output_as(model, model@io@output)
    ans <- predict(model, r_creators)
    expect_match(ans[[1L]]@first_name, "Robert|Ross")

    model <- output_as(model, model@io@output@schema)
    ans <- predict(model, "Text: R was created by Robert Gentleman.")
    expect_contains(ans$first_name, "Robert")

    model <- output_as(model, data.frame(created = logical()))
    ans <- predict(model, "Text: R was created by Robert Gentleman.")
    expect_true(ans$created)
})

test_that("glue templating works", {
    model <- test_model(server) |>
        instruct("Return exactly the word Able when asked for a 4-letter word starting with a.") |>
        prompt_as("Output a {nchar}-letter word starting with '{initial}'.")
    ans <- model |> predict(list(nchar = 4L, initial = "a"))
    expect_equal(trimws(ans), "Able")
    ans2 <- as.function(model)(4L, "a")
    expect_equal(ans, ans2)

    ans <- test_model(server) |>
        system_prompt_as(paste(
            "Answer questions about {language}.",
            "Use this fact exactly: R was created by Ross Ihaka and",
            "Robert Gentleman."
        )) |>
        chat(system_params = list(language = "R")) |>
        predict("Who created the language?")
    expect_match(ans, "Robert|Ross")
})
