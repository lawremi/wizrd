llamafile_version <- "0.8.13"

LlamaCppServer <- new_class("LlamaCppServer", OpenAIAPIServer,
                            properties = list(
                                model = prop_string,
                                embedding = prop_flag
                            ))

method(models, LlamaCppServer) <- function(x) data.frame(id = x@model)

method(language_model, LlamaCppServer) <-
    function(x, ..., params = language_model_params(...))
    {
        LanguageModel(backend = x, name = x@model, params = params)
    }

method(perform_chat, LlamaCppServer) <- function(x, ...) {
    if (x@embedding)
        stop("llama.cpp server does not chat when started with --embedding")
    perform_chat(super(x, OpenAIAPIServer), ...)
}

method(perform_embedding, LlamaCppServer) <- function(x, ...) {
    if (!x@embedding)
        stop("llama.cpp server does not embed when not started with --embedding")
    perform_embedding(super(x, OpenAIAPIServer), ...)
}

llama_cpp_model_from_ollama <- function(name, ...) {
    llama_cpp_model(ollama_weights_path(name), alias = name, ...)
}

llama_cpp_model <- function(path, mode = c("chat", "embedding"), alias = NULL,
                            ...)
{
    mode <- match.arg(mode)
    if (mode == "chat")
        server <- run_llama_cpp_server(path, alias = alias)
    else {
        server <- run_llamafiler(path)
        server@model <- alias
    }
    language_model(server, ...)
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

    Sys.chmod(dir(llamafile_bin_dir(), full.names = TRUE), "755")

    invisible(TRUE)
}

prompt_install_llamafile <- function() {
    answer <- if (interactive())
                  utils::askYesNo("Do you want to download llamafile in order to use llama.cpp models?")
              else TRUE
    
    if (isTRUE(answer)) {
        install_llamafile()
        TRUE
    } else FALSE
}

llamafile_ready <- function(stdout) {
    grepl("server listen", stdout)
}

llamafile_error <- function(stderr) {
    msgs <- gsub("error: (.*?)\n", "\\1", stderr)
    if (length(msgs) > 0L) tail(msgs, 1L) else stderr
}

llamafiler_error <- function(stderr) {
    msgs <- gsub("llamafiler: (?:error: )?(.*?)\n", "\\1", stderr)
    if (length(msgs) > 0L) tail(msgs, 1L) else stderr
}

llama_cpp_server <- function(model_name, url, embedding = FALSE, process = NULL)
{
    assert_string(model_name)
    assert_string(url)
    assert_flag(embedding)
    assert_class(process, "process", null.ok = TRUE)

    LlamaCppServer(url = url, model = model_name, embedding = embedding,
                   process = process)
}

.run_llama_cpp_server <- function(path = llamafile_path(),
                                  model = NULL, alias = NULL, embedding = FALSE,
                                  gpu = FALSE, port = 0L, ...)
{
    if (!file.exists(path) && identical(path, llamafile_path()))
        prompt_install_llamafile()
    assert_file_exists(path, access = "x")
    if (tools::file_ext(path) == "llamafile")
        assert_null(model)
    else assert_file_exists(model, "r")
    assert_string(model, null.ok = TRUE)
    assert_flag(embedding)
    assert_flag(gpu)
    if (identical(port, 0L))
        port <- find_available_port()
    assert_port(port)

    require_ns("processx", paste("run", basename(path)))

    if (is.null(alias)) {
        model_path <- if (!is.null(model)) model else path
        alias <- tools::file_path_sans_ext(basename(model_path))
    }
    is_llamafile <- basename(path) == "llamafile" ||
        tools::file_ext(path) == "llamafile"
    args <- make_args(server = TRUE, log_disable = TRUE, model = model,
                      alias = alias, embedding = embedding, port = port,
                      nobrowser = if (is_llamafile) TRUE,
                      ngl = if (gpu) 9999, ...)
    p <- init_llamafile_process(path, args)
    
    url <- paste0("http://localhost:", port)
    llama_cpp_server(alias, url, embedding, process = p)
}

init_llamafile_process <- function(path, args) {
    init_process(path, args, llamafile_ready, llamafile_error)
}

run_llamafiler <- function(model, port = 0L, path = llamafiler_path(), ...)
{
    if (!file.exists(path) && identical(path, llamafiler_path()))
        prompt_install_llamafile()
    assert_file_exists(path, access = "x")
    assert_file_exists(model, "r")
    if (identical(port, 0L))
        port <- find_available_port()
    assert_port(port)

    require_ns("processx", "run llamafiler")

    addr <- paste0("127.0.0.1:", port)
    url <- paste0("http://", addr)
    args <- make_args(log_disable = TRUE, model = model, listen = addr, ...)
    p <- init_llamafiler_process(path, args)
    
    model_name <- tools::file_path_sans_ext(basename(model))

    llama_cpp_server(model, url, embedding = TRUE, process = p)
}

init_llamafiler_process <- function(path, args) {
    init_process(path, args, llamafile_ready, llamafiler_error)
}

run_llamafile <- function(path = llamafile_path(), threads = 8L, port = 0L,
                          gpu = FALSE, ...) {
    .run_llama_cpp_server(path, threads = threads, port = port, gpu = gpu, ...)
}

run_llama_cpp_server <- function(model,
                                 threads = 8L, ctx_size = 512L,
                                 n_predict = -1L, batch_size = 512L,
                                 temp = 0.8, flash_attn = FALSE, port = 0L,
                                 embedding = FALSE, gpu = FALSE,
                                 path = llamafile_path(),
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
    
    .run_llama_cpp_server(path, model = model, threads = threads,
                          ctx_size = ctx_size, n_predict = n_predict,
                          batch_size = batch_size, temp = temp,
                          flash_attn = flash_attn, embedding = embedding,
                          gpu = gpu, port = port, ...)
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

method(on_restore, LlamaCppServer) <- function(x, ...) {
    init_fun <- if (x@embedding) init_llamafiler_process else init_llamafile_process
    p <- R6_private(x@process)
    set_props(x, process = init_fun(p$command, p$args))
}
