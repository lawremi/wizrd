test_that("models can generate embeddings", {
    model <- nomic()
    embedding <- embed_text(model, c("test", "test"))
    expect_equal(dim(embedding), c(2L, 768L))
})

test_that("RAG works with Rd", {
    chunks <- chunk(tools::Rd_db("S7"))
    store <- text_store(nomic(), chunks)
    file <- tempfile("text", fileext = ".rds")
    persist(store, file)
    store <- restore(file)
    model <- llama() |> prompt_as(results_augmented_query_to(store))
    ans <- predict(model, "new_property example")
    expect_match(ans, "new_property")
})

test_that("RAG works with vignettes", {
    vignettes <- utils::vignette(package = "S7")
    chunks <- chunk(vignettes)
    store <- text_store(nomic(), chunks)
    model <- llama() |> prompt_as(results_augmented_query_to(store))
    ans <- chat(model, "new_property example")
    expect_match(ans, "new_property")
})
