test_that("calling chat() and friends generates correct responses", {
    model <- llama_cpp_model_from_ollama(llama3()@name)

    chat <- chat(model, "How was R created, in three sentences?")
})

test_that("embedding works", {
    model <- llama_cpp_model_from_ollama(nomic()@name, mode = "embedding")
    
    embedding <- embed_text(model, "test")
    expect_equal(dim(embedding), c(1L, 768L))
}
