test_that("llama.cpp chatting works with a llamafile", {
    ans <- llamafile_llama() |>
        predict("How was R created, in three sentences?")
    expect_match(ans, "Robert.*Ross|Ross.*Robert")    
})

test_that("llama.cpp embedding works and we can reuse ollama weights", {
    model <- llama_cpp_model_from_ollama(nomic()@name, mode = "embedding")
    embedding <- embed_text(model, "test")
    expect_equal(dim(embedding), c(1L, 768L))
}
