test_that("models can call R functions as tools", {
    model <- llama(server)
    
    get_mean <- function(name) mean(get(name))
    model <- equip(model, get_mean, "Use to get the mean of an R variable whose name matches the 'name' argument.")
    var <- 1:10
    model@instructions <-
        "Always respond JSON, with field 'mean'. Only JSON. No text. No prefix."
    output <- predict(model, "What is the mean of var?")
    expect_equal(jsonlite::fromJSON(output)$mean, mean(var))

    model <- model |> unequip("get_mean") |>
        equip(tool(mean) |> describe_with_Rd() |> can_accept_as(class_name))
    output <- predict(model, "What is the mean of var? Respond in pure JSON.")
    expect_equal(jsonlite::fromJSON(output)$mean, mean(var))

    model <- equip(model, tool(mean) |> describe_with_Rd())
    output <- predict(model, "What is the mean of `var`? Return the mean in a JSON object.")
    expect_equal(jsonlite::fromJSON(output)$mean, mean(var))

    model <- llama(server)
    aggregate_tool <- tool(aggregate) |> describe_with_Rd() |>
        can_accept_as(x = class_formula, data = class_name, FUN = class_name)
    model <- equip(model, aggregate_tool)
    data(Cars93, package = "MASS")
    ans <- predict(model,
                  "Mean of MPG.city for each Manufacturer in the Cars93 dataset")
    expect_match(ans, "Lexus: 18")

    aggregate_tool <- tool(aggregate) |>
        demonstrate(c(x = MPG.city ~ Origin, alist(data = Cars93, FUN = median)),
                    "the median of MPG.city by Origin in Cars93")
    expect_equal(aggregate_tool@examples[[1L]][[1L]]@"_dots",
                 alist(data = Cars93, FUN = median))

    aggregate_tool <- tool(aggregate) |>
        can_accept_as(x = class_formula, data = class_name, FUN = class_name)
    expect_error(demonstrate(aggregate_tool,
                             alist(Cars93["MPG.city"], Cars93$Origin, median),
                             "the median of MPG.city by Origin in Cars93"))
})

test_that("We can make a model callable as a tool", {
    model <- openai_model("gpt-4o-mini", temperature = 0) |>
        prompt_as("Who created {language}?") |>
        output_as(data.frame(first_name = character(), last_name = character()))
    who_is_the_creator_of <- convert(model, wizrd:::Tool)
    ans <- who_is_the_creator_of(list(language = "R"))
    expect_contains(ans$first_name, "Robert")

    meanie <- llama(server) |>
        equip(tool(mean) |> describe_with_Rd() |>
                  can_accept_as(x = class_name)) |>
        instruct("Compute the mean of a named variable.",
                 "Assume the type of the variable to be nuzmeric.")
    ans <- openai_model("gpt-4o-mini", temperature = 0) |>
        equip(meanie, instructions = "Call to find the mean of a variable",
              predict_args = list(env = list2env(list(var = 1:3)))) |>
        predict("What is the mean of the variable 'var'?")
    expect_match(ans, "2")
})
