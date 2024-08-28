LanguageModelServer <- new_class("LanguageModelServer",
                                 properties = list(
                                     url = prop_string,
                                     key_prefix = prop_string,
                                     process = NULL | new_S3_class("process")
                                 ),
                                 abstract = TRUE)

method(language_model, LanguageModelServer) <-
    function(x, name, ..., params = language_model_params(...)) {
        RemoteLanguageModel(server = x, name = name, params = params)
    }

req_auth_fun <- new_generic("req_auth_fun", "server")

create_request <- function(server) {
    req <- httr2::request(server@url)
    if (nzchar(server@key_prefix))
        req_auth_fun(server)(req, get_api_key(server@key_prefix))
    else req
}

method(str, LanguageModelServer) <- function(object, ...) {
    cat(S7:::obj_desc(object), "")
    cat(object@url)
    cat("\n")
}

## TODO ([X] done):

## Commercial: [X] OpenAI, Gemini, Anthropic, Perplexity
## Local / open-source: [X] LlamaCpp (server, llamafile), [X] Ollama
## Third party hosts: Hugging Face, Bedrock, Azure, Vertex AI
