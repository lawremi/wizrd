LanguageModelServer <- new_class("LanguageModelServer",
                                 properties = list(
                                     url = prop_string,
                                     key_prefix = prop_string_nullable
                                 )),
                                 abstract = TRUE)

method(language_model, LanguageModelServer) <- function(x, name) {
    RemoteLanguageModel(server = x, name = name)
}

req_auth_fun <- new_generic("req_auth_fun", "server")

create_request <- function(server) {
    req <- httr2::request(server@url)
    if (!is.null(server@key_prefix))
        req_auth_fun(server)(req, get_api_key(server@key_prefix))
    else req
}

## TODO ([X] done):

## Commercial: [X] OpenAI, Gemini, Anthropic, Perplexity
## Local / open-source: [X] LlamaCpp (server, llamafile), Ollama
## Third party hosts: Hugging Face, Bedrock, Azure, Vertex AI
