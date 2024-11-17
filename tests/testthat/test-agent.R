test_that("We can create a simple agent", {
    model <- llama3() |> accept_as("Who created {language}?") |>
        output_as(data.frame(first_name = character(), last_name = character()))
    
    agt <- agent(model)
    ans <- agt(list(language = "R"))

    mean_agt <- llama3() |> equip(mean) |>
        instruct("Compute the mean of a variable") |> agent()

    ans <- llama3() |> equip(mean_agt) |> predict("What is the mean of `var`?")
})
