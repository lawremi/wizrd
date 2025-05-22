llamafile_version <- "0.9.0"

LlamaCppServer <- new_class("LlamaCppServer", OpenAIAPIServer,
                            properties = list(
                                model = scalar(class_character),
                                embedding = scalar(class_logical)
                            ))

method(models, LlamaCppServer) <- function(x) data.frame(id = x@model)

method(language_agent, LlamaCppServer) <-
    function(x, ..., params = LanguageModelParams(...)) {
        Agent(backend = x, name = x@model, params = params)
    }

method(perform_chat, LlamaCppServer) <- function(x, ...) {
    if (x@embedding)
        stop("llama.cpp server does not chat when started with --embedding")
    perform_chat(super(x, OpenAIAPIServer), ...)
}

method(perform_embedding, LlamaCppServer) <- function(x, ...) {
    if (!x@embedding)
        stop("llama.cpp must be started with --embedding")
    perform_embedding(super(x, OpenAIAPIServer), ...)
}

llama_cpp_agent_from_ollama <- function(name, ...) {
    llama_cpp_agent(ollama_weights_path(name), alias = name, ...)
}

llama_cpp_agent <- function(path, mode = c("chat", "embedding"), alias = NULL,
                            server_path = NULL, ...) {
    mode <- match.arg(mode)

    if (resembles_url(path))
        path <- cache_llama_cpp_agent(path)

    if (tools::file_ext(path) == "llamafile") {
        model <- NULL
        server_path <- path
    } else {
        model <- path
        server_path <- server_path %||% llamafile_path()
    }

    run_server <- if (mode == "chat") run_llama_cpp_server else run_llamafile_v2
    server <- run_server(model, alias = alias, path = server_path)
    language_agent(server, ...)
}

cache_llama_cpp_agent <- function(url) {
    path <- cache_file(url, "models")
    if (tools::file_ext(path) == "llamafile")
        Sys.chmod(path, "755")
    path
}

llamafile_llama <- function(temperature = 0, ...) {
    llama_cpp_agent("https://huggingface.co/Mozilla/Llama-3.2-3B-Instruct-llamafile/resolve/main/Llama-3.2-3B-Instruct.Q6_K.llamafile")
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

install_llamafile <- function() {
    url <- llamafile_url()

    dest_file <- tempfile(fileext = ".zip")
    download_file(url, dest_file)

    user_dir <- tools::R_user_dir("wizrd", which = "cache")
    dir.create(user_dir, recursive = TRUE, showWarnings = FALSE)

    unzip(dest_file, exdir = user_dir)
    unlink(dest_file)

    Sys.chmod(dir(llamafile_bin_dir(), full.names = TRUE), "755")

    invisible(TRUE)
}

prompt_install_llamafile <- function() {
    answer <- if (interactive()) {
        askYesNo("Download llamafile to use llama.cpp models?")
    } else {
        TRUE
    }

    if (isTRUE(answer)) {
        install_llamafile()
        TRUE
    } else {
        FALSE
    }
}

llamafile_ready <- function(stdout) {
    grepl("server listen", stdout)
}

llamafile_error <- function(stderr) {
    msgs <- gsub("error: (.*?)\n", "\\1", stderr)
    if (length(msgs) > 0L) tail(msgs, 1L) else stderr
}

llamafile_v2_error <- function(stderr) {
    msgs <- gsub("llamafile: (?:error: )?(.*?)\n", "\\1", stderr)
    if (length(msgs) > 0L) tail(msgs, 1L) else stderr
}

llama_cpp_server <- function(model_name, url, embedding = FALSE,
                             process = NULL) {
    assert_string(model_name)
    assert_string(url)
    assert_flag(embedding)
    assert_class(process, "process", null.ok = TRUE)

    LlamaCppServer(url = url, model = model_name, embedding = embedding,
                   process = process)
}

.run_llama_cpp_server <- function(path = llamafile_path(),
                                  model = NULL, alias = NULL, embedding = FALSE,
                                  gpu = FALSE, port = 0L, ...) {
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

run_llamafile_v2 <- function(model, port = 0L, path = llamafile_path(),
                             alias = NULL, ...) {
    if (!file.exists(path) && identical(path, llamafile_path()))
        prompt_install_llamafile()
    assert_file_exists(path, access = "x")
    if (tools::file_ext(path) == "llamafile")
        assert_null(model)
    else assert_file_exists(model, "r")
    if (identical(port, 0L))
        port <- find_available_port()
    assert_port(port)

    require_ns("processx", "run llamafile")

    addr <- paste0("127.0.0.1:", port)
    url <- paste0("http://", addr)
    args <- make_args(log_disable = TRUE, server = TRUE, v2 = TRUE,
                      model = model, listen = addr, ...)
    p <- init_llamafile_v2_process(path, args)

    model_name <- alias %||%
        tools::file_path_sans_ext(basename(model %||% path))

    llama_cpp_server(model_name, url, embedding = TRUE, process = p)
}

init_llamafile_v2_process <- function(path, args) {
    init_process(path, args, llamafile_ready, llamafile_v2_error)
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
                                 ...) {
    assert_integerish(threads, lower = 1L)
    assert_integerish(ctx_size, lower = 0L)
    assert_integerish(n_predict, lower = -2L)
    assert_integerish(batch_size, lower = 1L)
    assert_number(temp, lower = 0)
    assert_flag(flash_attn)

    .run_llama_cpp_server(path, model = model, threads = threads,
                          ctx_size = ctx_size, n_predict = n_predict,
                          batch_size = batch_size, temp = temp,
                          flash_attn = flash_attn, embedding = embedding,
                          gpu = gpu, port = port, ...)
}

method(on_restore, LlamaCppServer) <- function(x, ...) {
    init_fun <- if (x@embedding) {
        init_llamafile_v2_process
    } else {
        init_llamafile_process
    }
    p <- R6_private(x@process)
    set_props(x, process = init_fun(p$command, p$args))
}
