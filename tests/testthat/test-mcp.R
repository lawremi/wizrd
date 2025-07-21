test_that("we can call MCP tools", {
    session <- connect_mcp(wizrd:::start_test_mcp())
    mcp_tools <- tools(session)
    result <- mcp_tools$add(1L, 2L)
    expect_match(result, "3")

    model <- llama(server)
    result <- model |>
        equip(mcp_tools) |>
        predict("use the add tool to add 1 and 2")
    expect_identical(result, "The result of adding 1 and 2 is 3.")
})

test_that("we can access MCP resources", {
    session <- connect_mcp(wizrd:::start_test_mcp())
    r <- resources(session)
    result <- r$get_greeting("R")
    expect_identical(result, "Hello, R!")
})

test_that("we can generate prompts with MCP", {
    session <- connect_mcp(wizrd:::start_test_mcp())
    pf <- prompts(session)

    model <- llama(server)

    result <- model |>
        prompt_as(pf$ask_review) |>
        predict(list(code_snippet = "print \"foo\""))
    expect_match(result, "Python 3.0", fixed = TRUE)

    expect_warning(
        result <- model |>
            prompt_as(pf$ask_review) |>
            predict(list(code_snippet = "for (i in 1:length(n)) v <- c(v, i)",
                         language = "R"))
    )
    expect_match(result, "R code snippet", fixed = TRUE)

    error_message <-
        "Error in if (x) y else x (from #1) : argument is of length zero"
    msgs <- pf$debug_session_start(error_message)
    info <- "Trying to use if() in R. Traceback: 1: fun(integer())"
    result <- predict(model, c(msgs, info))
    expect_match(result, "empty vector", fixed = TRUE)
})

test_that("the SSE transport layer works", {
    is_mocking <- !is.null(getOption("httr2_mock"))
    if (is_mocking) { # the dynamic POST URL confuses httptest2
        httptest2::stop_mocking()
        on.exit(httptest2::use_mock_api())
    }

    port <- wizrd:::find_available_port()
    mcp_server <- wizrd:::start_test_mcp("sse", port)
    url <- paste0("http://127.0.0.1:", port, "/sse")

    session <- connect_mcp(url)
    r <- resources(session)
    result <- r$get_greeting("R")
    expect_identical(result, "Hello, R!")
})

test_that("the Streamable HTTP transport layer works", {
    port <- wizrd:::find_available_port()
    mcp_server <- wizrd:::start_test_mcp("http", port)
    url <- paste0("http://127.0.0.1:", port, "/mcp")

    session <- connect_mcp(url)
    r <- resources(session)
    result <- r$get_greeting("R")
    expect_identical(result, "Hello, R!")
})

test_that("MCP errors work", {
    session <- connect_mcp(wizrd:::start_test_mcp())
    pf <- prompts(session)

    model <- llama(server)

    expect_error(model |>
                     prompt_as(pf$ask_review) |>
                     predict(list(code_snippet = "", language = "R")))
})

test_that("MCP requests can be cancelled", {
    skip("Not sure if FastMCP yet handles notifications/cancelled")
    options(wizrd_verbose = TRUE)
    session <- connect_mcp(wizrd:::start_test_mcp())
    mcp_tools <- tools(session)
    opt <- options(wizrd_test_json_rpc_cancel = TRUE)
    on.exit(options(opt))
    result <- mcp_tools$sleep()
})

test_that("dynamic oauth works", {
    skip_if_not(interactive())
    mcp_connect("https://server.smithery.ai/@jzinno/biomart-mcp/mcp")
})
