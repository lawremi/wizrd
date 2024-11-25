test_that("We can create a simple agent", {
    model <- openai_model() |> accept_as("Who created {language}?") |>
        output_as(data.frame(first_name = character(), last_name = character()))
    agt <- agentize(model)
    ans <- agt(list(language = "R"))
    expect_contains(ans$first_name, "Robert")

    var <- 1:3
    meanie := llama3() |> equip(tool(mean) |> can_accept_as(x = class_name)) |>
        instruct("Compute the mean of a variable") |> agentize()
    ans <- openai_model() |>
        equip(meanie, instructions = "Call to find the mean of a variable") |>
        predict("What is the mean of the variable 'var'?")
    expect_match(ans, "2")
})
