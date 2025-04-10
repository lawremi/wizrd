test_that("we can call MCP tools", {
    options(wizrd_verbose = TRUE)
    session <- mcp_connect(wizrd:::mcp_test_server())
    mcp_tools <- tools(session)
    result <- mcp_tools$add(1L, 2L)
    expect_match(result, "3")

    model <- llama(server)
    result <- model |> equip(mcp_tools) |>
        predict("use the add tool to add 1 and 2")
    expect_match(result, "The result of adding 1 and 2 is 3.")
})
