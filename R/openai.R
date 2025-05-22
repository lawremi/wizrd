openai_server <- function(url = getOption("wizrd.openai.url",
                                          "https://api.openai.com"),
                          key_prefix = "OPENAI") {
    OpenAIAPIServer(url = url, key_prefix = key_prefix)
}

openai_agent <- function(name = "gpt-4o-mini", ...) {
    language_agent(openai_server(), name, ...)
}
