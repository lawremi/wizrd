test_that("We can create a simple agent", {
    model <- openai_model() |> accept_as("Who created {language}?") |>
        output_as(data.frame(first_name = character(), last_name = character()))
    
    agt <- agent(model)
    ans <- agt(list(language = "R"))

    var <- 1:3
    mean_agt <- llama3() |> equip(tool(mean) |> can_accept_as(x = class_name)) |>
        instruct("Compute the mean of a variable") |> agent("meanie")

    ans <- openai_model() |> equip(mean_agt) |>
        predict("What is the mean of `var`?")
})
