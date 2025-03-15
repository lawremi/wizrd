test_that("structured output works", {
    model <- openai_model(temperature = 0) |>
        output_as(data.frame(first_name = character(), last_name = character()))
    ans <- predict(model, "Who created R?")
    expect_contains(ans$first_name, "Robert")
    expect_contains(ans$last_name, "Gentleman")

    Person <- S7::new_class("Person", properties = list(
        first_name = S7::class_character,
        last_name = S7::class_character
    ))
    model <- output_as(model, Person)
    ans <- predict(model, "Who created R?")
    expect_contains(ans@first_name, "Robert")
    expect_contains(ans@last_name, "Gentleman")

    Person <- S7::new_class("Person", properties = list(
        first_name = wizrd:::prop_string,
        last_name = wizrd:::prop_string
    ))
    model <- output_as(model, wizrd:::new_list_property(of = Person))
    ans <- predict(model, "Who created R?")
    expect_match(ans[[1L]]@first_name, "Robert|Ross")

    model <- output_as(model, model@io@output)
    ans <- predict(model, "Who created R?")
    expect_match(ans[[1L]]@first_name, "Robert|Ross")

    model <- output_as(model, model@io@output@schema)
    ans <- predict(model, "Who created R?")
    expect_contains(ans$first_name, "Robert")
    
    model <- output_as(model, TRUE) # unconstrained JSON output
    ans <- predict(model, "Who created R?")
    expect_true(ans) # obviously wrong, but...
})

test_that("glue templating works", {
    model <- llama(server) |> instruct("Return a single word.") |>
        prompt_as("Output a {nchar}-letter word starting with '{initial}'.")
    ans <- model |> predict(list(nchar = 4L, initial = "a"))
    expect_equal(trimws(ans), "Able")
    ans2 <- as.function(model)(4L, "a")
    expect_equal(ans, ans2)

    ans <- llama(server) |>
        system_prompt_as("Answer questions about {language}") |>
        chat(system_params = list(language = "R")) |>
        predict("Who created the language?")
    expect_match(ans, "Robert|Ross")
})

