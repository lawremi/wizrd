OllamaServer <- new_class("OllamaServer", LanguageModelServer,
                          properties = list(
                              url = new_string_property(
                                  default = "http://localhost:11434/api"
                              ),
                              process = NULL | class_process
                          ))

ollama_url <- function() {
    host <- Sys.getenv("OLLAMA_HOST", "127.0.0.1:11434")
    paste0("http://", host, "/api")
}

ollama_server <- function(...) {
    server <- OllamaServer(url = ollama_url())
    if (!ollama_is_running(server))
        start_ollama_server(server, ...)
    else server
}

start_ollama_server <- function(server = ollama_server(),
                                path = Sys.which("ollama"),
                                max_seconds = 10L)
{
    requireNamespace("processx", "run ollama")

    assert_class(server, "OllamaServer")
    if (missing(path) && identical(path, ""))
        stop("Cannot find the ", sQuote("ollama"), " binary. Ensure that ",
             "it is installed and on the $PATH.")
    assert_file_exists(path, "x")
    
    p <- processx::process$new(path, "serve")

    wait_until_ready(set_props(server, process = p), max_seconds)
}

ollama_is_running <- function(server = ollama_server()) {
    !inherits(try(ollama_list(server), silent = TRUE), "try-error")
}

ollama_list <- function(server = ollama_server()) {
    assert_class(server, "OllamaServer")
    httr2::request(server@url) |> httr2::req_url_path_append("tags") |>
        httr2::req_perform() |> httr2::resp_body_json() |> fromJSON()
}

ollama_model <- function(name, pull = FALSE, server = ollama_server()) {
    assert_flag(pull)
    if (pull)
        system2("ollama", c("pull", name))
    language_model(server, name)
}

poll_path <- new_generic("poll_path", "server")

method(poll_path, OllamaServer) <- function(server) "tags"

wait_until_ready <- function(server, max_seconds) {
    assert_int(max_seconds, lower = 0L)
    create_request(server) |> httr2::req_url_path_append(poll_path(server)) |>
        httr2::req_retry(max_seconds = max_seconds) |> httr2::perform()
    server
}

