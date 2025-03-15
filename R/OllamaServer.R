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

ollama_server <- function(url = ollama_url(), start = TRUE, ...) {
    server <- OllamaServer(url = url)
    if (start && !ollama_is_running(server))
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

ollama_is_installed <- function() {
    nzchar(Sys.which("ollama"))
}

ollama_is_running <- function(server = ollama_server(start = FALSE)) {
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
    invisible(TRUE)
}

require_ollama_model <- function(name, server = ollama_server(), pull = NA) {
    assert_flag(pull, na.ok = TRUE)
    assert_string(name)

    installed <- name %in% ollama_list(server)$name
    if (installed)
        return(invisible(TRUE))
    
    if (is.na(pull))
        pull <- !interactive() || askYesNo(paste0("Pull ", name, "?"))
    
    invisible(pull && ollama_pull(name, server))
}

maybe_add_latest <- function(name) {
    if (!grepl(":", name))
        paste0(name, ":latest")
    else name
}

ollama_model <- function(name, pull = NA, server = ollama_server(), ...) {
    name <- maybe_add_latest(name)
    if (!require_ollama_model(name, server, pull))
        stop("Model '", name, "' not found")
    language_model(server, name, ...)
}

ollama_dir <- function() {
    file.path(home_dir(), ".ollama")
}

ollama_weights_path <- function(name) {
    assert_string(name)
    ollama <- ollama_dir()
    lib <- file.path(ollama, "models", "manifests", "registry.ollama.ai",
                     "library")
    manifest_path <- file.path(lib, sub(":", .Platform$file.sep, name))
    if (!file.exists(manifest_path))
        require_ollama_model(name)
    manifest <- fromJSON(manifest_path)
    digest <- manifest$layers$digest[manifest$layers$mediaType ==
                                         "application/vnd.ollama.image.model"]
    stopifnot(length(digest) == 1L)
    blobs <- file.path(ollama, "models", "blobs")
    file.path(blobs, sub(":", "-", digest))
}

wait_until_ready <- function(server, max_seconds) {
    assert_integerish(max_seconds, lower = 0L)
    httr2::request(server@url) |> httr2::req_url_path_append("api", "tags") |>
        httr2::req_retry(max_seconds = max_seconds, retry_on_failure = TRUE) |>
        httr2::req_perform()
    server
}

method(on_restore, OllamaServer) <- function(x, name, ...) {
    server <- ollama_server(x@url)
    require_ollama_model(name, server)
    server
}

ollama_llama_vision <- function(server, temperature = 0, ...) {
    ollama_model("llama3.2-vision:11b-instruct-q4_K_M",
                 server = server, temperature = temperature, ...)
}

ollama_llama <- function(server, temperature = 0, ...)
{
    ans <- try(ollama_model("llama3.2:3b-instruct-q4_K_M",
                            server = server, temperature = temperature, ...))
    if (inherits(ans, "try-error"))
        stop("Try using llamafile_llama() as a quickstart.")
    ans
}

ollama_nomic <- function(server, temperature = 0, ...) {
    ollama_model("nomic-embed-text:v1.5", temperature = temperature,
                 server = server, ...)
}
