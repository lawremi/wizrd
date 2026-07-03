test_that("models can generate embeddings", {
    model <- embedding_model(server)
    embedding <- embed_text(model, c("test", "test"))
    expect_equal(dim(embedding), c(2L, 768L))
})

test_that("RAG works with Rd", {
    chunks <- chunk(tools::Rd_db("S7"))
    store <- text_store(embedding_model(server), chunks)
    file <- tempfile("text", fileext = ".rds")
    persist(store, file)
    store <- restore(file)
    model <- test_model(server) |> prompt_as(rag_with(store))
    ans <- predict(model, "new_property example")
    expect_match(ans, "new_property")
})

test_that("RAG works with vignettes", {
    vignettes <- utils::vignette(package = "S7")
    chunks <- chunk(vignettes)
    store <- text_store(embedding_model(server), chunks)
    model <- test_model(server) |> prompt_as(rag_with(store))
    ans <- predict(model, "new_property example")
    expect_match(ans, "new_property")
})

test_that("RAG works with data.frames", {
    store <- text_store(embedding_model(), chunk(mtcars))
    model <- test_model() |> prompt_as(rag_with(store))
    ans <- predict(model, "MPG of Datsun 710")
    expect_match(ans, "22.8")
})
