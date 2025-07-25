test_that("llama.cpp chatting works with a llamafile", {
    ans <- llamafile_llama(port = 8000L) |>
        predict("How was R created, in three sentences?")
    expect_match(ans, "Robert.*Ross|Ross.*Robert")
})

test_that("llama.cpp supports external weights and generating embeddings", {
    model <- wizrd:::llama_cpp_agent_from_ollama(nomic(server)@name,
                                                 mode = "embedding",
                                                 temperature = 0,
                                                 port = 8001L)
    embedding <- embed_text(model, "test")
    expect_equal(dim(embedding), c(1L, 768L))
})
