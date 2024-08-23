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
    llama_cpp_model(system.file("extdata", "stories260K.gguf",
                                package = "wizrd"))
}

llama_cpp_model <- function(path, ...) {
    language_model(start_llama_cpp_server(path, ...))
}

llamafile_ready <- function(stdout) {
    grepl("llama server listening", stdout)
}

llamafile_error <- function(stderr) {
    m <- gregexec("error: (.*?)\n", stderr)
    msgs <- regmatches(stderr, m)[[1L]][2L,]
    if (length(msgs) > 0L) tail(msgs, 1L) else stderr
}

.run_llamafile <- function(path = system.file("bin", "llamafile",
                                              package = "wizrd"),
                           model = NULL, gpu = FALSE, port = 0L,
                           max_seconds = 10L, ...)
{
    require_ns("processx", "run llamafile")
    
    assert_file_exists(path, access = "x")
    assert_string(model, null.ok = TRUE)
    assert_flag(gpu)
    if (identical(port, 0L)) {
        port <- find_available_port()
    }
    assert_int(port, lower = 1024L, upper = 65535L)
    
    args <- make_args(server = TRUE, nobrowser = TRUE, log_disable = TRUE,
                      model = model, port = port, ngl = if (gpu) 9999, ...)
    p <- init_process(path, args, llamafile_ready, llamafile_error)
    
    model_path <- if (!is.null(model)) model else path
    model_name <- tools::file_path_sans_ext(basename(model_path))
    
    LlamaCppServer(url = paste0("http://localhost:", port),
                   model = model_name, process = p)
}

run_llamafile <- function(path = system.file("bin", "llamafile",
                                             package = "wizrd"),
                          port = 0L, gpu = FALSE, max_seconds = 10L, ...) {
    .run_llamafile(path, port = port, gpu = gpu, max_seconds = max_seconds, ...)
}

start_llama_cpp_server <- function(model,
                                   threads = 8L, ctx_size = 0L,
                                   n_predict = -1L, batch_size = 2048L,
                                   temp = 0.8, flash_attn = FALSE, port = 0L,
                                   embedding = FALSE, gpu = FALSE,
                                   max_seconds = 10L,
                                   llamafile = system.file("bin", "llamafile",
                                                           package = "wizrd"),
                                   ...)
{
    assert_file_exists(model, access = "r")
    assert_int(threads, lower = 1L)
    assert_int(ctx_size, lower = 0L)
    assert_int(n_predict, lower = -2L)
    assert_int(batch_size, lower = 1L)
    assert_number(temp, lower = 0)
    assert_flag(flash_attn)
    assert_flag(embedding)
    
    .run_llamafile(llamafile, model = model, threads = threads,
                   ctx_size = ctx_size, n_predict = n_predict,
                   batch_size = batch_size, temp = temp,
                   flash_attn = flash_attn, embedding = embedding, gpu = gpu,
                   port = port, max_seconds = max_seconds, ...)
}

poll_path <- new_generic("poll_path", "server")

method(poll_path, LlamaCppServer) <- function(server) "health"

wait_until_ready <- function(server, max_seconds) {
    assert_int(max_seconds, lower = 0L)
    create_request(server) |> httr2::req_url_path_append(poll_path(server)) |>
        httr2::req_retry(max_seconds = max_seconds) |> httr2::perform()
    server
}

health <- function(server) {
    assert_class(server, LlamaCppServer)
    create_request(server) |> httr2::req_url_path_append("health") |>
        httr2::req_perform()
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
