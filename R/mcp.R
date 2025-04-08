MCP_VERSION <- "2024-11-05"

MCPSession <- setRefClass("MCPSession",
    fields = list(
        endpoint = "ANY"
    ),
    methods = list(
        ## 'initialize' is a reserved method
        mcp_initialize = function(.self) {
            invoke(MCPInitializeRequest(), .self)
            notify(MCPInitializedNotification(), .self)
        },
        listTools = function(.self) {
            invoke(MCPListToolsRequest(), .self)
        },
        finalize = function(.self) {
            close(.self)
        }
    )
)

MCPSSEEndpoint := new_class(
    properties = list(
        sse_con = S3_connection,
        post_url = scalar(class_character)
    )
)

MCPImplementation := new_class(
    properties = list(
        name = scalar(class_character,
                      default = getNamespaceName(environment())),
        version = scalar(class_character,
                         default = getNamespaceVersion(environment()))
    ))

MCPClientCapabilities := new_class(
    properties = list(
        experimental = nullable(named(class_list)),
        sampling = NULL | class_list,
        roots = NULL | class_list
    ))

MCPServerCapabilities := new_class(
    properties = list(
        experimental = nullable(named(class_list)),
        logging = NULL | class_list,
        completions = NULL | class_list,
        prompts = NULL | class_list,
        resources = NULL | class_list,
        tools = NULL | class_list
    ))

MCPRequest := new_class()

MCPResult := new_class()

MCPNotification := new_class(MCPRequest)

MCPInitializeRequest := new_class(
    MCPRequest,
    properties = list(
        protocolVersion = scalar(class_character, default = MCP_VERSION),
        capabilities = MCPClientCapabilities,
        clientInfo = MCPImplementation
    )
)

MCPInitializeResult := new_class(
    MCPResult,
    properties = list(
        protocolVersion = scalar(class_character, default = MCP_VERSION),
        capabilities = MCPServerCapabilities,
        serverInfo = MCPImplementation,
        instructions = scalar(NULL | class_character)
    )
)

MCPInitializedNotification := new_class(
    MCPNotification
)

MCPPaginatedRequest := new_class(
    MCPRequest,
    properties = list(
        cursor = scalar(NULL | class_character)
    )
)

MCPListToolsRequest := new_class(
    MCPPaginatedRequest
)

MCPToolAnnotations := new_class(
    properties = list(
        readOnlyHint = scalar(class_logical, default = FALSE),
        destructiveHint = scalar(class_logical, default = TRUE),
        idempotentHint = scalar(class_logical, default = FALSE),
        openWorldHint = scalar(class_logical, default = TRUE)
    )
)

MCPTool := new_class(
    properties = list(
        name = scalar(class_character),
        description = scalar(NULL | class_character),
        inputSchema = class_list,
        annotations = MCPToolAnnotations
    )
)

MCPListToolsResult := new_class(
    MCPResult,
    properties = list(
        tools = list_of(MCPTool)
    )
)

MCPPingRequest := new_class()

close.MCPSession <- function(con, ...) {
    close(con$endpoint, ...)
}

method(json_rpc_method, MCPInitializeRequest) <- function(x) "initialize"

method(json_rpc_method, MCPListToolsRequest) <- function(x) "tools/list"

method(json_rpc_method, MCPPingRequest) <- function(x) "ping"

method(json_rpc_method, MCPNotification) <- function(x) {
    name <- tolower(sub(".*MCP(.*)Notification", "\\1", class(x)[1L]))
    paste0("notifications/", name)
}

method(json_rpc_params, MCPRequest) <- function(x) props(x)

method(response_prototype, MCPInitializeRequest) <-
    function(x) MCPInitializeResult()

method(response_prototype, MCPListToolsRequest) <-
    function(x) MCPListToolsResult()

method(send, list(class_any, MCPSession)) <- function(x, to) {
    send(x, to$endpoint)
    to
}

method(send, list(MCPRequest, class_any)) <- function(x, to) {
    convert(x, JSONRPCRequest) |> send(to)
    to
}

method(send, list(MCPNotification, class_any)) <- function(x, to) {
    convert(x, JSONRPCNotification) |> send(to)
    to
}

method(receive, list(class_any, MCPResult)) <- function(from, as, ...) {
    receive(from, JSONRPCResponse()) |> receive(as, ...)
}

method(receive, list(JSONRPCResponse, MCPResult)) <- function(from, as, ...) {
    from@result |> receive(as, ...)
}

method(receive, list(MCPSession, class_any)) <- function(from, as, ...) {
    receive(from$endpoint, as, ...)
}

ws_endpoint <- function(url) {
    require_ns("websocket", "connect to websocket-based MCP servers")
    BufferedWebSocket(webSocket = WebSocket$new(url))
}

mcp_endpoint <- function(server) {
    if (is.character(server) && resembles_url(server, "ws"))
        ws_endpoint(server)
    else server
}

mcp_connect <- function(server) {
    session <- MCPSession(endpoint = mcp_endpoint(server))
    session$mcp_initialize()
    session
}

need_builtin_uvx <- function(command, args) {
    command == "uvx" && !file.exists(command) && Sys.which(command) == ""
}

uv_pipex <- function(command, args) {
    require_ns("reticulate", "find/install builtin uv")
    uv_pipe_tool <- reticulate::uv_run_tool
    environment(uv_pipe_tool) <- list2env(list(system2 = pipex),
                                          parent = environment(uv_pipe_tool))
    uv_pipe_tool(command, args)
}

try_uv_pipex <- function(command, args) {
    if (command == "uv" && identical(head(args, 2L), c("tool", "run"))) {
        command <- "uvx"
        args <- tail(args, -2L)
    }
    if (need_builtin_uvx(command, args))
        uv_pipex(args[1L], args[-1L])
}

mcp_exec_server <- function(command, args) {
    assert_string(command)
    assert_character(args, any.missing = FALSE)
    require_ns("processx", "connect to stdio-based MCP servers")
    try_uv_pipex(command, args) %||% pipex(command, args)
}

mcp_test_server <- function() {
    mcp_exec_server("uvx", c("fastmcp", "run", 
                             system.file("mcp", "server.py", package = "wizrd")))
}
