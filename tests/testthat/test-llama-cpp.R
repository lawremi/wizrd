test_that("calling chat() and friends generates correct responses", {
    model <- llama3()

    model <- wizrd:::llama_cpp_chat_model_from_ollama(model@name)
    chat <- chat(model, "How was R created, in three sentences?")
})
