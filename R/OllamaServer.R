OllamaServer <- new_class("OllamaServer", OpenAIAPIServer,
                          properties = list(
                              url = new_string_property(
                                  default = "http://localhost:11434"
                              )
                          ))

ollama_url <- function() {
    host <- Sys.getenv("OLLAMA_HOST", "127.0.0.1:11434")
    paste0("http://", host)
}

ollama_server <- function(url = ollama_url(), ...) {
    server <- OllamaServer(url = url)
    if (!ollama_is_running(server))
        start_ollama_server(server, ...)
    else server
}

start_ollama_server <- function(server, path = Sys.which("ollama"),
                                max_seconds = 10L)
{
    require_ns("processx", "run ollama")

    stopifnot(inherits(server, OllamaServer))
    if (missing(path) && !nzchar(path))
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
    stopifnot(inherits(server, OllamaServer))
    to_df <- function(x) {
        do.call(rbind, lapply(x$models, \(m) {
            m$details$families <- I(list(unlist(m$details$families)))
            as.data.frame(m)
        }))
    }
    httr2::request(server@url) |> httr2::req_url_path_append("api", "tags") |>
        httr2::req_perform() |> httr2::resp_body_json() |> to_df()
}

method(models, OllamaServer) <- function(x) {
    ollama_list(x) |> rename(name = "id")
}

ollama_pull <- function(name, server = ollama_server()) {
    stopifnot(inherits(server, OllamaServer))
    system2("ollama", c("pull", name))
}

maybe_ollama_pull <- function(pull, name, server = ollama_server()) {
    assert_flag(pull, na.ok = TRUE)
    assert_string(name)
        
    pull <- if (is.na(pull)) {
        installed <- name %in% ollama_list(server)$name
        if (!installed) {
            if (interactive())
                utils::askYesNo(paste0("Pull ", name, "?"))
            else TRUE
        } else FALSE
    }
    if (pull)
        ollama_pull(name, server)

    pull
}

maybe_add_latest <- function(name) {
    if (!grepl(":", name))
        paste0(name, ":latest")
    else name
}

ollama_model <- function(name, pull = NA, server = ollama_server(), ...) {
    name <- maybe_add_latest(name)
    maybe_ollama_pull(pull, name, server)
    language_model(server, name, ...)
}

ollama_weights_path <- function(name) {
    assert_string(name)
    lib <- path.expand("~/.ollama/models/manifests/registry.ollama.ai/library/")
    manifest_path <- file.path(lib, sub(":", .Platform$file.sep, name))
    if (!file.exists(manifest_path))
        stop("no manifest found for ", name)
    manifest <- fromJSON(manifest_path)
    digest <- manifest$layers$digest[manifest$layers$mediaType ==
                                         "application/vnd.ollama.image.model"]
    stopifnot(length(digest) == 1L)
    blobs <- path.expand("~/.ollama/models/blobs")
    file.path(blobs, sub(":", "-", digest))
}

wait_until_ready <- function(server, max_seconds) {
    assert_int(max_seconds, lower = 0L)
    httr2::request(server@url) |> httr2::req_url_path_append("api", "tags") |>
        httr2::req_retry(max_seconds = max_seconds) |> httr2::req_perform()
    server
}

method(on_restore, OllamaServer) <- function(x, name, ...) {
    server <- ollama_server(x@url)
    maybe_ollama_pull(NA, name, server)
    server
}

llama3 <- function(temperature = 0, ...) {
    ollama_model("llama3.1:8b-instruct-q4_K_M", temperature = temperature, ...)
}

llava <- function(temperature = 0, ...) {
    ollama_model("llava", temperature = temperature, ...)
}

nomic <- function(temperature = 0, ...) {
    ollama_model("nomic-embed-text:v1.5", temperature = temperature, ...)
}
