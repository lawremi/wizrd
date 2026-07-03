test_that("models can call R functions as tools", {
    model <- test_model(server)

    get_mean <- function(name) mean(get(name))
    model <- equip(model, get_mean,
                   "Use to get the mean of an R variable whose name matches the 'name' argument.")
    var <- 1:10
    model@instructions <- paste(
        "You must call get_mean for any mean calculation.",
        "After receiving the tool result, your entire final answer must be",
        "exactly one JSON object like {\"mean\":5.5}.",
        "Do not add prose, markdown, or units."
    )
    output <- predict(model, "What is the mean of the R variable named var?")
    expect_equal(jsonlite::fromJSON(output)$mean, mean(var))

    model <- model |>
        unequip("get_mean") |>
        equip(tool(mean) |> describe_with_Rd() |> can_accept_as(class_name))
    model@instructions <- paste(
        "You are in an R session. The user may name R variables.",
        "You must call mean for any mean calculation.",
        "Pass the R variable name as the x argument.",
        "After receiving the tool result, your entire final answer must be",
        "exactly one JSON object like {\"mean\":5.5}.",
        "Do not add prose, markdown, or units."
    )
    output <- predict(model, "What is the mean of var?")
    expect_equal(jsonlite::fromJSON(output)$mean, mean(var))

    mean_tool <- tool(mean) |> describe_with_Rd()
    mean_tool@param_descriptions["x"] <- "The name of the variable"
    model <- equip(model, mean_tool)
    model@instructions <- paste(
        "You must call mean for any mean calculation.",
        "After receiving the tool result, your entire final answer must be",
        "exactly one JSON object like {\"mean\":5.5}.",
        "Do not add prose, markdown, or units."
    )
    output <- predict(model, "What is the mean of `var`?")
    expect_equal(jsonlite::fromJSON(output)$mean, mean(var))

    model <- test_model(server)
    aggregate_tool <- tool(aggregate) |>
        describe_with_Rd() |>
        can_accept_as(x = class_formula, data = class_name, FUN = class_name)
    model <- equip(model, aggregate_tool)
    data(Cars93, package = "MASS")
    ans <- predict(model,
                  "Mean of MPG.city for each Manufacturer in the Cars93 dataset")
    expect_match(ans, "Lexus.*18")

    aggregate_tool <- tool(aggregate) |>
        demonstrate(c(x = MPG.city ~ Origin,
                      alist(data = Cars93, FUN = median)),
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
    model <- test_model(server) |>
        instruct("Use this fact exactly: R was created by Ross Ihaka and Robert Gentleman.") |>
        prompt_as("Who created {language}?") |>
        output_as(data.frame(first_name = character(), last_name = character()))
    who_is_the_creator_of <- convert(model, wizrd:::Tool)
    ans <- who_is_the_creator_of(list(language = "R"))
    expect_contains(ans$first_name, "Robert")

    meanie <- test_model(server) |>
        equip(tool(mean) |> 
                describe_with_Rd() |>
                  can_accept_as(x = class_name)) |>
        instruct("Compute the mean of a named variable.",
                 "Assume the type of the variable to be nuzmeric.")
    ans <- test_model(server) |>
        equip(meanie, instructions = "Call to find the mean of a variable",
              predict_args = list(env = list2env(list(var = 1:3)))) |>
        predict("What is the mean of the variable 'var'?")
    expect_match(ans, "2")
})
