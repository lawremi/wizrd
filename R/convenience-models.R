## Test and demonstration models for chat, vision and text embedding

llama <- function(server = ollama_server(), temperature = 0, ...) {
    ollama_llama(server, temperature, ...)
}

vision_model <- function(server = ollama_server(), temperature = 0, ...) {
    ollama_gemma(server, temperature, ...)
}

llama_vision <- vision_model

embedding_model <- function(server = ollama_server(), temperature = 0, ...) {
    ollama_nomic(server, temperature, ...)
}

nomic <- embedding_model

quick_start_model <- function(temperature = 0, ...) {
    llamafile_ministral(temperature, ...)
}

gemma <- function(server = ollama_server(), temperature = 0, ...) {
    ollama_gemma(server, temperature, ...)
}

test_model <- gemma
