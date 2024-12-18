test_that("llama.cpp chatting works", {
    model <- llama_cpp_model_from_ollama(llama3()@name)
    ans <- predict(model, "How was R created, in three sentences?")
    expect_match(ans, "Robert.*Ross|Ross.*Robert")
})

test_that("llama.cpp embedding works", {
    model <- llama_cpp_model_from_ollama(nomic()@name, mode = "embedding")
    embedding <- embed_text(model, "test")
    expect_equal(dim(embedding), c(1L, 768L))
}
