LanguageModelServer <- new_class("LanguageModelServer", LanguageModelBackend,
                                 properties = list(
                                     url = prop_string,
                                     key_prefix = prop_string,
                                     process = NULL | new_S3_class("process")
                                 ),
                                 abstract = TRUE)

models <- new_generic("models", "x")

language_model <- new_generic("language_model", "x")

method(language_model, LanguageModelBackend) <-
    function(x, name, ..., params = language_model_params(...)) {
        LanguageModel(backend = x, name = name, params = params)
    }

chat_completions_path <- new_generic("chat_completions_path", "server",
                                     function(server, model) S7_dispatch())
embeddings_path <- new_generic("embeddings_path", "server",
                               function(server, model) S7_dispatch())
models_path <- new_generic("models_path", "server")

add_api_key <- new_generic("add_api_key", "server",
                           function(server, req, key) S7_dispatch())

add_api_version <- new_generic("add_api_version", "server",
                            function(server, req) S7_dispatch())

create_request <- function(server) {
    req <- httr2::request(server@url) |> httr2::req_retry(max_tries = 10L)
    if (nzchar(server@key_prefix))
        req <- add_api_key(server, req, get_api_key(server@key_prefix))
    add_api_version(server, req)
}

method(print, LanguageModelServer) <- function(x, ...) {
    cat(S7:::obj_desc(x), "")
    cat(x@url)
    cat("\n")
}

## TODO ([X] done):

## Commercial: [X] OpenAI, Gemini, Anthropic, Perplexity
## Local / open-source: [X] LlamaCpp (server, llamafile), [X] Ollama
## Third party hosts: Hugging Face, Bedrock, Azure, Vertex AI
