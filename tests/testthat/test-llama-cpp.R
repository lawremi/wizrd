test_that("llama.cpp chatting works with a llamafile", {
    ans <- wizrd:::llamafile_ministral(port = 18080L, max_tokens = 64L) |>
        predict("Reply with exactly: wizrd llama.cpp OK")
    expect_match(ans, "wizrd llama.cpp OK", fixed = TRUE)
})

test_that("stop_model stops server processes", {
    r <- file.path(R.home("bin"),
                   if (.Platform$OS.type == "windows") "Rterm.exe" else "R")
    p <- processx::process$new(r, c("--slave", "-e", "Sys.sleep(60)"))
    on.exit(if (p$is_alive()) p$kill(), add = TRUE)

    server <- wizrd:::llama_cpp_server("test", "http://127.0.0.1:9",
                                       process = p)
    expect_true(p$is_alive())
    expect_true(stop_model(server))
    p$wait(1000)
    expect_false(p$is_alive())
})

test_that("llama.cpp supports external weights and generating embeddings", {
    model <- wizrd:::llama_cpp_agent_from_ollama(embedding_model(server)@name,
                                                 mode = "embedding",
                                                 temperature = 0,
                                                 port = 18081L)
    embedding <- embed_text(model, "test")
    expect_equal(dim(embedding), c(1L, 768L))
})
