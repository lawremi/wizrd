test_that("mcp works", {
    cmd <- "bash -c 'source .venv/bin/activate && fastmcp run ~/tmp/server.R'"
    session <- mcp_connect(cmd)
    session$listTools()
})
