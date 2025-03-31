MCPSession := new_class(
    properties = list(
        endpoint = class_any
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

mcp_method := new_generic()

method(mcp_method, MCPInitializeRequest) <- function(x) "initialize"

method(mcp_method, MCPListToolsRequest) <- function(x) "tools/list"

method(jsonify, MCPRequest) <- function(x) {
    lapply(props(x), jsonify)
}

method(send, list(class_any, MCPSession)) <- function(x, to) {
    send(x, to@endpoint)
}

method(send, list(MCPRequest, class_any)) <- function(x, to) {
    JSONRPCRequest(method = mcp_method(x), params = x)
}

method(receive, list(class_character, MCPResult)) <- function(from, as) {
    receive(from, JSONRPCResponse(result = as))@result
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

method(response_prototype, MCPInitializeRequest) <-
    function(x) MCPInitializeResult()

mcp_connect <- function(server) {
    ## 'server' can be either a command (stdio assumed) or a http:// url (SSE)
    session <- MCPSession(endpoint = mcp_endpoint(server))
    session$initialize()
    session
}
