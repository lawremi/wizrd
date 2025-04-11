test_that("we can call MCP tools", {
    session <- mcp_connect(wizrd:::mcp_test_server())
    mcp_tools <- tools(session)
    result <- mcp_tools$add(1L, 2L)
    expect_match(result, "3")

    model <- llama(server)
    result <- model |> equip(mcp_tools) |>
        predict("use the add tool to add 1 and 2")
    expect_match(result, "The result of adding 1 and 2 is 3.")
})

test_that("we can access MCP resources", {
    session <- mcp_connect(wizrd:::mcp_test_server())
    r <- resources(session)
    result <- r$get_greeting("R")
    expect_match(result, "Hello, R!")
})

test_that("we can generate prompts with MCP", {
    options(wizrd_verbose = TRUE)
    session <- mcp_connect(wizrd:::mcp_test_server())
    pf <- prompt_formats(session)

    model <- llama(server)

    result <- model |> prompt_as(pf$ask_review) |>
        predict(list(code_snippet = "print \"foo\""))

    error_message <-
        "Error in if (x) y else x (from #1) : argument is of length zero"
    traceback <- "1: fun(integer())"
    msgs <- pf$debug_session_start(error_message)
    result <- chat(model, msgs) |> predict(traceback)
    expect_match(result, "")
})
