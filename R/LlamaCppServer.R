class_process <- new_S3_class("process")

LlamaCppServer <- new_class("LlamaCppServer", OpenAIAPIServer,
                            properties = list(
                                model = prop_string,
                                process = NULL | class_process
                            ))

method(language_model, LlamaCppServer) <- function(x) {
    RemoteLanguageModel(server = x, name = x@model)
}

stories260K <- function() {
    llama_cpp_model(system.file("wizrd", "extdata", "stories260K.gguf"))
}

llama_cpp_model <- function(path, ...)
{
    language_model(start_llama_cpp_server(path, ...))
}

.run_llamafile <- function(path = system.file("wizrd", "bin", "llamafile"),
                           model = NULL, port = 0L,
                           max_seconds = 10L, ...)
{
    requireNamespace("processx")
    
    assert_file_exists(path, access = "x")
    assert_string(model, null.ok = TRUE)
    if (identical(port, 0L)) {
        port <- find_available_port()
    }
    assert_int(port, lower = 1024L, upper = 65535L)
    
    args <- make_args(model = model, port = port, ...)
    p <- processx::process$new(path, args)

    model_path <- if (!is.null(model)) model else path
    model_name <- tools::file_path_sans_ext(basename(model_path))
    
    server <- LlamaCppServer(url = paste0("http://localhost:", port),
                             model = model_name, process = p)
    wait_until_ready(server, max_seconds)
}

run_llamafile <- function(path = system.file("wizrd", "bin", "llamafile"),
                          port = 0L, max_seconds = 10L, ...) {
    .run_llamafile(path, port = port, max_seconds = max_seconds, ...)
}

start_llama_cpp_server <- function(model,
                                   threads = 8L, ctx_size = 0L,
                                   predict = -1L, batch_size = 2048L,
                                   temp = 0.8, flash_attn = FALSE, port = 0L,
                                   embedding = FALSE, max_seconds = 10L,
                                   llamafile = system.file("wizrd", "bin",
                                                           "llamafile"),
                                   ...)
{
    assert_file_exists(model, access = "r")
    assert_int(threads, lower = 1L)
    assert_int(ctx_size, lower = 0L)
    assert_int(predict, lower = -2L)
    assert_int(batch_size, lower = 1L)
    assert_number(temp, lower = 0)
    assert_flag(flash_attn)
    assert_flag(embedding)
    
    .run_llamafile(llamafile, model = model, threads = threads,
                   ctx_size = ctx_size, predict = predict,
                   batch_size = batch_size, temp = temp,
                   flash_attn = flash_attn, embedding = embedding,
                   json_schema = json_schema, host = host, port = port,
                   max_seconds = max_seconds, ...)
}

poll_path <- new_generic("poll_path", "server")

method(poll_path, LlamaCppServer) <- function(server) "health"

wait_until_ready <- function(server, max_seconds) {
    assert_int(max_seconds, lower = 0L)
    create_request(server) |> req_url_path_append(poll_path(server)) |>
        req_retry(max_seconds = max_seconds)
    server
}

health <- function(server) {
    assert_class(server, LlamaCppServer)
    create_request(server) |> req_url_path_append("health") |> req_perform()
}

find_available_port <- function(start = 8000, end = 8100) {
    stopifnot(start <= end)
    for (port in start:end) {
        socket <- try(serverSocket(port), silent = TRUE)
        if (!inherits(socket, "try-error")) {
            close(socket)
            return(port)
        }
    }
    stop("No available ports found in ", start, ":", end)
}
