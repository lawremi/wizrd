test_that("RAG works", {
    chunks <- chunk_Rd("S7")
    store <- text_store(nomic(), chunks)
    model <- llama3() |> accept_as(results_augmented_query_to(store))
    ans <- predict(model, "Short example of defining a new property with S7")
    expect_match(ans, "new_property")
})
