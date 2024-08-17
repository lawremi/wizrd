openai_server <- function(url = "https://api.openai.com", key_prefix = "OPENAI")
{
    OpenAIAPIServer(url = url, key_prefix = key_prefix)
}

openai_model <- function(name, server = openai_server(...), ...) {
    language_model(server, name)
}
