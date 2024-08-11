LanguageModelServer <- new_class("LanguageModelServer",
                                 properties = list(
                                     url = scalar(class_character),
                                     key_prefix =
                                         nullable(scalar(class_character))
                                 ),
                                 abstract = TRUE)

method(llm, LanguageModelServer) <- function(x, name) {
    assert_string(name)
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
## Local / open-source: [X] LlamaCpp (server), Ollama, llamafile
## Third party hosts: Hugging Face, Bedrock, Azure, Vertex AI
