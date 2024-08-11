LlamaCppServer <- new_class("LlamaCppServer", OpenAIAPIServer,
                            properties = list(
                                model = class_character
                            ),
                            validator = function(object) {
                                validate_string(object@model)
                            })

method(llm, LlamaCppServer) <- function(x) {
    llm(super(x, LanguageModelServer), name = x@model)
}

start_llama_cpp_server <- function(path = Sys.which("llama-server"),
                                   model = system.file("wizrd", "extdata",
                                                       "stories260K.gguf"),
                                   threads = 8L, ctx_size = 0L,
                                   predict = -1L, batch_size = 2048L,
                                   temp = 0.8, flash_attn = FALSE,
                                   host = "localhost", port = 8080L,
                                   embedding = FALSE,
                                   max_seconds = 10L, ...)
{
    if (missing(path) && identical(path, ""))
        stop(sQuote("llama-server"), " is not on the $PATH. Ensure that ",
             "llama.cpp is installed ",
             "(https://github.com/ggerganov/llama.cpp/releases) and its ",
             "binaries are on the $PATH.")
    assert_file_exists(path, access = "x")
    assert_string(model)
    assert_int(threads, min = 1L)
    assert_int(ctx_size, min = 0L)
    assert_int(predict, min = -2L)
    assert_int(batch_size, min = 1L)
    assert_number(temp, min = 0)
    assert_flag(flash_attn)
    assert_flag(embedding)
    assert_string(host)
    assert_int(port, min = 1024L, max = 65535L)
    assert_int(max_seconds, min = 0L)

    cmd <- make_command(path, model = model, threads = threads,
                        ctx_size = ctx_size, predict = predict,
                        batch_size = batch_size, temp = temp,
                        flash_attn = flash_attn, embedding = embedding,
                        json_schema = json_schema, host = host, port = port,
                        ...)
    system(cmd)

    model_name <- tools::file_path_sans_ext(basename(model))
    server <- LlamaCppServer(url = paste0("http://", host, ":", port),
                             model = model_name)
    wait_until_ready(server, max_seconds)
}

poll_path <- new_generic("poll_path", "server")

method(poll_path, LlamaCppServer) <- function(server) "health"

wait_until_ready <- function(server, max_seconds) {
    create_request(server) |> req_url_path_append(poll_path(server)) |>
        req_retry(max_seconds = max_seconds)
    server
}

health <- function(server) {
    assert_class(server, LlamaCppServer)
    create_request(server) |> req_url_path_append("health") |> req_perform()
}
