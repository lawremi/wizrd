test_that("mcp works", {
    options(wizrd_verbose = TRUE)
    server <- wizrd:::mcp_test_server()
    session <- mcp_connect(server)
    session$listTools()
    
})
