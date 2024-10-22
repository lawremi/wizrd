test_that("calling predict() generates correct responses", {
    model <- openai_model(temperature = 0)
    model@instructions <- "Respond with a single sentence"

    msg <- predict(model, "Who created R?")
    expect_match(msg, "Robert.*Ross|Ross.*Robert")
})

test_that("structured input/output works", {
    model <- openai_model(temperature = 0)
    Person <- S7::new_class("Person",
                            properties = list(
                                first_name = S7::class_character,
                                last_name = S7::class_character
                            ))
    person <- model |> output_as(Person) |> predict("Who created R?")
    expect_equal(person, Person(c("Robert", "Ross"), c("Gentleman", "Ihaka")))

    df <- data.frame(first_name = character(), last_name = character())
    person <- model |> output_as(df) |> predict("Who created R?")
    expect_equal(person, data.frame(first_name = c("Ross", "Robert"),
                                    last_name = c("Ihaka", "Gentleman")))    
})
