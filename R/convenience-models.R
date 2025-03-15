## Test and demonstration models for chat, vision and text embedding

llama <- function(server = ollama_server(), temperature = 0, ...) {
    ollama_llama(server, temperature, ...)
}

llama_vision <- function(server = ollama_server(), temperature = 0, ...) {
    ollama_llama_vision(server, temperature, ...)
}

nomic <- function(server = ollama_server(), temperature = 0, ...) {
    ollama_nomic(server, temperature, ...)
}
