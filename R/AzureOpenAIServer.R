AzureOpenAIServer <- new_class("AzureOpenAIServer", OpenAIAPIServer)

AZURE_OPENAI_API_VERSION <- "2024-08-01-preview"

azure_openai_server <- function(url = getOption("wizrd_azure_openai_url"),
                                key_prefix = "AZURE_OPENAI") {
    AzureOpenAIServer(url = url, key_prefix = key_prefix)
}

azure_openai_agent <- function(name = "gpt-4o",
                               url = getOption("wizrd_azure_openai_url"), ...) {
    language_agent(azure_openai_server(url), name, ...)
}

method(server_req_api_key, AzureOpenAIServer) <- function(server, req, key) {
    httr2::req_headers(req, "api-key" = key)
}

method(chat_completions_path, AzureOpenAIServer) <- function(server, model) {
    paste0("openai/deployments/", model, "/chat/completions")
}

method(embeddings_path, AzureOpenAIServer) <- function(server, model) {
    paste0("openai/deployments/", model, "/embeddings")
}

method(models_path, AzureOpenAIServer) <- function(server) {
    "openai/models"
}

method(server_req_api_version, AzureOpenAIServer) <- function(server, req) {
    httr2::req_url_query(req, "api-version" = AZURE_OPENAI_API_VERSION)
}
