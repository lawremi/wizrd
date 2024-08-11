OllamaServer <- new_class("OllamaServer", LanguageModelServer,
                          properties = list(
                              url = new_property(class_character,
                                                 default =
                                                    "http://localhost:11434/api")
                          ))

start_ollama_server <- function(path = Sys.which("ollama"), max_seconds = 10L)
{
    if (missing(path) && identical(path, ""))
        stop("Cannot find the ", sQuote("ollama"), " binary. Ensure that ",
             "it is installed and on the $PATH.")

    system(paste(path, "serve"))

    wait_until_ready(OllamaServer(), max_seconds)
}

method(poll_path, OllamaServer) <- function(server) "tags"
