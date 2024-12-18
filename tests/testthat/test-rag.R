test_that("RAG works with Rd", {
    chunks <- chunk(tools::Rd_db("S7"))
    store <- text_store(nomic(), chunks)
    file <- tempfile("text", fileext = ".rds")
    persist(store, file)
    store <- restore(file)
    model <- llama3() |> prompt_as(results_augmented_query_to(store))
    ans <- predict(model, "new_property example")
    expect_match(ans, "new_property")
})

test_that("RAG works with vignettes", {
    vignettes <- utils::vignette(package = "S7")
    chunks <- chunk(vignettes)
    store <- text_store(nomic(), chunks)
    model <- llama3() |> prompt_as(results_augmented_query_to(store))
    ans <- chat(model, "new_property example")
    expect_match(ans, "new_property")
})
