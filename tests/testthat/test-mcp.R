test_that("mcp works", {
    session <- mcp_connect(wizrd:::mcp_test_server())
    session$listTools()
})
