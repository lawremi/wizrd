test_that("RAG works with Rd", {
    chunks <- chunk_Rd("S7")
    store <- text_store(nomic(), chunks)
    file <- tempfile("text", fileext = ".rds")
    persist(store, file)
    store <- restore(file)
    model <- llama3() |> accept_as(results_augmented_query_to(store))
    ans <- predict(model, "Short example of defining a new property with S7")
    expect_match(ans, "new_property")
})

test_that("RAG works with vignettes", {
    vignettes <- utils::vignette(package = "S7")
    chunk(vignettes)
})
