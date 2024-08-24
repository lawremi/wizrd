llamafile_version <- "0.8.13"

class_process <- new_S3_class("process")

LlamaCppServer <- new_class("LlamaCppServer", OpenAIAPIServer,
                            properties = list(
                                model = prop_string,
                                process = NULL | class_process
                            ))

method(language_model, LlamaCppServer) <- function(x) {
    RemoteLanguageModel(server = x, name = x@model)
}

stories260K <- function(...) {
    path <- system.file("extdata", "stories260K.gguf", package = "wizrd")
    model <- llama_cpp_model(path, n_predict = 400L, ...)
    model@instructions <- "You are a storyteller"
    model
}

llama_cpp_model <- function(path, ...) {
    language_model(start_llama_cpp_server(path, ...))
}

llamafile_url <- function() {
    paste0("https://github.com/Mozilla-Ocho/llamafile/releases/download/",
           llamafile_version, "/llamafile-", llamafile_version, ".zip")
}

llamafile_bin_dir <- function() {
    file.path(tools::R_user_dir("wizrd", which = "cache"),
              paste0("llamafile-", llamafile_version), "bin")
}

llamafile_path <- function() {
     file.path(llamafile_bin_dir(), "llamafile")
}

llamafiler_path <- function() {
     file.path(llamafile_bin_dir(), "llamafiler")
}

install_llamafile <- function() {
    url <- llamafile_url()

    dest_file <- tempfile(fileext = ".zip")
    download.file(url, dest_file, mode = "wb")

    user_dir <- tools::R_user_dir("wizrd", which = "cache")
    dir.create(user_dir, recursive = TRUE, showWarnings = FALSE)
    
    unzip(dest_file, exdir = user_dir)
    unlink(dest_file)

    Sys.chmod(dir(llama_file_bin_dir(), full.names = TRUE), "755")

    invisible(TRUE)
}

prompt_install_llamafile <- function() {
    answer <- if (interactive())
                  utils::askYesNo("Do you want to download llamafile?")
              else TRUE
    
    if (isTRUE(answer)) {
        install_llamafile()
        TRUE
    } else FALSE
}

llamafile_ready <- function(stdout) {
    grepl("llama server listening", stdout)
}

llamafile_error <- function(stderr) {
    m <- gregexec("error: (.*?)\n", stderr)
    msgs <- regmatches(stderr, m)[[1L]][2L,]
    if (length(msgs) > 0L) tail(msgs, 1L) else stderr
}

.run_llamafile <- function(path = llamafile_path(),
                           model = NULL, gpu = FALSE, port = 0L,
                           max_seconds = 10L, ...)
{
    require_ns("processx", "run llamafile")

    if (!file.exists(path) && identical(path, llamafile_path()))
        prompt_install_llamafile()
    assert_file_exists(path, access = "x")
    assert_string(model, null.ok = TRUE)
    assert_flag(gpu)
    if (identical(port, 0L))
        port <- find_available_port()
    assert_int(port, lower = 1024L, upper = 65535L)
    
    args <- make_args(server = TRUE, nobrowser = TRUE, log_disable = TRUE,
                      model = model, port = port, ngl = if (gpu) 9999, ...)
    p <- init_process(path, args, llamafile_ready, llamafile_error)
    
    model_path <- if (!is.null(model)) model else path
    model_name <- tools::file_path_sans_ext(basename(model_path))
    
    LlamaCppServer(url = paste0("http://localhost:", port),
                   model = model_name, process = p)
}

run_llamafile <- function(path = llamafile_path(), port = 0L, gpu = FALSE,
                          max_seconds = 10L, ...) {
    .run_llamafile(path, port = port, gpu = gpu, max_seconds = max_seconds, ...)
}

start_llama_cpp_server <- function(model,
                                   threads = 8L, ctx_size = 0L,
                                   n_predict = -1L, batch_size = 2048L,
                                   temp = 0.8, flash_attn = FALSE, port = 0L,
                                   embedding = FALSE, gpu = FALSE,
                                   max_seconds = 10L,
                                   llamafile = llamafile_path(),
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
