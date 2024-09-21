AzureOpenAIServer <- new_class("AzureOpenAIServer", OpenAIAPIServer)

AZURE_OPENAI_API_VERSION <- "2024-06-01"

azure_openai_server <- function(url = getOption("wizrd.azure.openai.url"),
                                key_prefix = "AZURE_OPENAI")
{
    AzureOpenAIServer(url = url, key_prefix = key_prefix)
}

azure_openai_model <- function(name = "gpt-4o", ...) {
    language_model(azure_openai_server(), name, ...)
}

method(add_api_key, AzureOpenAIServer) <- function(server, req, key) {
    httr2::req_headers(req, "api-key" = key)
}

method(chat_completions_path, AzureOpenAIServer) <- function(server, model) {
    paste0("openai/deployments/", model, "/chat/completions")
}

method(add_api_version, AzureOpenAIServer) <- function(server, req) {
    httr2::req_url_query(req, "api-version" = AZURE_OPENAI_API_VERSION)
}
