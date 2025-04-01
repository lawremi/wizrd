MCPSession := new_class(
    properties = list(
        endpoint = class_any
    )
)

MCPSSEEndpoint := new_class(
    properties = list(
        sse_con = S3_connection,
        post_url = scalar(class_string)
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
        experimental = new_list_property(named = TRUE) | NULL,
        sampling = class_list | NULL,
        roots = class_list | NULL
    ))

MCPServerCapabilities := new_class(
    properties = list(
        experimental = new_list_property(named = TRUE) | NULL,
        logging = class_list | NULL,
        completions = class_list | NULL,
        prompts = class_list | NULL,
        resources = class_list | NULL,
        tools = class_list | NULL
    ))

MCPRequest := new_class()

MCPResult := new_class()

MCPInitializeRequest := new_class(
    MCPRequest,
    properties = list(
        protocolVersion = scalar(class_character),
        capabilities = MCPClientCabilities,
        clientInfo = MCPImplementation
    )
)

MCPInitializeResult := new_class(
    MCPResult,
    properties = list(
        protocolVersion = scalar(class_character | class_integer),
        capabilities = MCPServerCapabilities,
        serverInfo = MCPImplementation,
        instructions = scalar(class_character | NULL)
    )
)

MCPPaginatedRequest := new_class(
    properties = list(
        cursor = scalar(class_character | NULL)
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
        description = scalar(class_character | NULL)
        inputSchema = class_list,
        annotations = MCPToolAnnotations
    )
)

MCPListToolsResult := new_class(
    MCPResult,
    properties = list(
        tools = new_list_property(MCPTool)
    )
)

method(json_rpc_method, MCPInitializeRequest) <- function(x) "initialize"

method(json_rpc_method, MCPListToolsRequest) <- function(x) "tools/list"

method(json_rpc_params, MCPRequest) <- function(x) props(x)

method(response_prototype, MCPInitializeRequest) <-
    function(x) MCPInitializeResult()

method(send, list(class_any, MCPSession)) <- function(x, to) {
    send(x, to@endpoint)
}

method(send, list(MCPRequest, class_any)) <- function(x, to) {
    convert(x, JSONRPCRequest) |> send(to)
}

method(receive, list(class_any, MCPResult)) <- function(from, as) {
    receive(from, JSONRPCResponse()) |> receive(as)
}

method(receive, list(JSONRPCResponse, MCPResult)) <- function(from, as) {
    from@result |> receive(as)
}

mcp_request_constructor <- function(name) {
    substring(name, 1L) <- toupper(substring(name, 1L, 1L))
    match.fun(paste0(name, "MCPRequest"))
}

method(`$`, MCPSession) <- function(x, name) {
    fun <- mcp_request_constructor(name)
    CONSTRUCTOR_CALL <- as.call(lapply(c(name, names(formals(fun))),
                                       as.name))
    body(fun) <- substitute({
        CONSTRUCTOR_CALL |> invoke(session)
    })
    environment(fun) <- list2env(list(session = x), topenv())
    fun
}

want_reticulate_uv <- function(cmd) {
    if ((startsWith(cmd, "uv run ") || startsWith(cmd, "uvx ")) &&
            !file.exists("uv") && Sys.which("uv") == "")
        return(NULL)
    install <- !interactive() ||
        askYesNo("uv not found; use reticulate to find/install?")
    if (install)
        require_ns("reticulate", "find/install uv")
}

make_uv_pipe_tool <- function() {
    uv_pipe_tool <- reticulate::uv_run_tool
    environment(uv_pipe_tool) <- list2env(list(
        system2 = function(command, args) {
            pipe(paste(command, paste(args, collapse = " ")))
        }
    ), environment(uv_pipe_tool))
    uv_pipe_tool
}

uv_pipe <- function(cmd) {
    if (!want_reticulate_uv(cmd))
        return(NULL)
    args <- strsplit(cmd, " ", fixed = TRUE)[[1L]] |> tail(-2L)
    make_uv_pipe_tool()(args[1L], args[-1L])
}

pipe_endpoint <- function(cmd) {
    uv_pipe(cmd) %||% pipe(cmd)
}

ws_endpoint <- function(url) {
    require_ns("websocket", "connect to websocket-based MCP servers")
    BufferedWebSocket(webSocket = WebSocket$new(url))
}

mcp_endpoint <- function(server) {
    assert_string(server)
    if (resembles_url(server, "ws"))
        ws_endpoint(server)
    else pipe_endpoint(server)
}

mcp_connect <- function(server) {
    session <- MCPSession(endpoint = mcp_endpoint(server))
    session$initialize()
    session
}
