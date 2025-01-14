## Test and demonstration models for chat, vision and text embedding

llama <- function(temperature = 0, ...) {
    ollama_llama(temperature, ...)
}

llama_vision <- function(temperature = 0, ...) {
    ollama_llama_vision(temperature, ...)
}

nomic <- function(temperature = 0, ...) {
    ollama_nomic(temperature, ...)
}
