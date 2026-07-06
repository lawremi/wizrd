test_that("llama.cpp chatting works with a llamafile", {
    ans <- wizrd:::llamafile_ministral(port = 18080L, max_tokens = 64L) |>
        predict("Reply with exactly: wizrd llama.cpp OK")
    expect_match(ans, "wizrd llama.cpp OK", fixed = TRUE)
})

test_that("llama.cpp supports external weights and generating embeddings", {
    model <- wizrd:::llama_cpp_agent_from_ollama(embedding_model(server)@name,
                                                 mode = "embedding",
                                                 temperature = 0,
                                                 port = 18081L)
    embedding <- embed_text(model, "test")
    expect_equal(dim(embedding), c(1L, 768L))
})
