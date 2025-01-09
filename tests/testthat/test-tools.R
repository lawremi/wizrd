test_that("models can call R functions as tools", {
    model <- ollama_llama()
    
    get_mean <- function(name) mean(get(name))
    model <- equip(model, get_mean, "Use to get the mean of an R variable")
    var <- 1:10
    model@instructions <-
        "Always respond JSON, with field 'mean'. Only JSON. No text. No prefix."
    output <- predict(model, "What is the mean of var?")
    expect_equal(jsonlite::fromJSON(output)$mean, mean(var))

    model <- model |> unequip("get_mean") |>
        equip(tool(mean) |> can_accept_as(class_name))
    output <- predict(model, "What is the mean of var?")
    expect_equal(jsonlite::fromJSON(output)$mean, mean(var))

    model <- equip(model, mean)
    output <- predict(model, "What is the mean of `var`?")
    expect_equal(jsonlite::fromJSON(output)$mean, mean(var))

    aggregate_tool <- tool(method(aggregate, class_formula)) |>
        can_accept_as(x = class_formula, data = class_name, FUN = class_name,
                      subset = class_call) |>
        can_output_as(class_data.frame)
    model <- unequip("mean") |> equip(model, aggregate_tool)
    output <- predict(model, "Mean of MPG.city for each Manufacturer in the Cars93 dataset")
    expect_equal(as.numeric(output), mean(var))
})

test_that("We can make a model callable as a tool", {
    model <- openai_model() |> prompt_as("Who created {language}?") |>
        output_as(data.frame(first_name = character(), last_name = character()))
    agt <- convert(model, wizrd:::Tool)
    ans <- agt(list(language = "R"))
    expect_contains(ans$first_name, "Robert")

    var <- 1:3
    meanie := ollama_llama() |>
        equip(tool(mean) |> can_accept_as(x = class_name)) |>
        instruct("Compute the mean of a variable")
    ans <- openai_model() |>
        equip(meanie, instructions = "Call to find the mean of a variable") |>
        predict("What is the mean of the variable 'var'?")
    expect_match(ans, "2")
})
