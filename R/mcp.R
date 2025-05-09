MCP_VERSION <- "2025-03-26"

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
            tools <- tryCatch(
                invoke(MCPListToolsRequest(), .self)@tools,
                JSONRPCMethodNotFound = function(cond) list()
            )
            ans <- tools |> lapply(convert, Tool, .self)
            setNames(ans, vapply(ans, \(xi) xi@name, character(1L)))
        },
        callTool = function(.self, name, arguments) {
            invoke(MCPCallToolRequest(name = name,
                                      arguments = jsonify(arguments)),
                   .self) |> textify()
        },
        listResources = function(.self) {
            resources <- tryCatch(
                invoke(MCPListResourcesRequest(), .self)@resources,
                JSONRPCMethodNotFound = function(cond) list()
            )
            ans <- lapply(resources, as.function, .self)
            setNames(ans, vapply(resources, \(xi) xi@name, character(1L)))
        },
        listResourceTemplates = function(.self) {
            resourceTemplates <- tryCatch(
                invoke(MCPListResourceTemplatesRequest(),
                       .self)@resourceTemplates,
                JSONRPCMethodNotFound = function(cond) list()
            )
            ans <- lapply(resourceTemplates, as.function, .self)
            setNames(ans, vapply(resourceTemplates, \(xi) xi@name,
                                 character(1L)))
        },
        readResource = function(.self, uri) {
            result <- invoke(MCPReadResourceRequest(uri = uri), .self)
            ans <- lapply(result@contents, textify)
            if (length(ans) == 1L)
                ans <- ans[[1L]]
            ans
        },
        listPrompts = function(.self) {
            prompts <- tryCatch(
                invoke(MCPListPromptsRequest(), .self)@prompts,
                JSONRPCMethodNotFound = function(cond) list()
            )
            ans <- lapply(prompts, as.function, .self)
            setNames(ans, vapply(prompts, \(xi) xi@name, character(1L)))
        },
        getPrompt = function(.self, name, arguments) {
            request <- MCPGetPromptRequest(name = name, arguments = arguments)
            result <- invoke(request, .self)
            lapply(result@messages, convert, ChatMessage)
        },
        show = function(.self) {
            cat("<MCPSession>")
            cat("\n@endpoint: "); print(.self$endpoint)
        },
        finalize = function(.self) {
            close(.self)
        }
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
        experimental = optional(named(class_list)),
        sampling = optional(class_list),
        roots = optional(class_list)
    ))

MCPServerCapabilities := new_class(
    properties = list(
        experimental = nullable(named(class_list)),
        logging = optional(class_list),
        completions = optional(class_list),
        prompts = optional(class_list),
        resources = optional(class_list),
        tools = optional(class_list)
    ))

MCPRequest := new_class(abstract = TRUE)

MCPResult := new_class(abstract = TRUE)

MCPNotification := new_class(MCPRequest, abstract = TRUE)

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
        instructions = optional(scalar(class_character))
    )
)

MCPInitializedNotification := new_class(
    MCPNotification
)

MCPPaginatedRequest := new_class(
    MCPRequest,
    properties = list(
        cursor = optional(scalar(class_character))
    ),
    abstract = TRUE
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
        description = optional(scalar(class_character)),
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

MCPCallToolRequest := new_class(
    MCPRequest,
    properties = list(
        name = scalar(class_character),
        arguments = class_list
    )
)

Role <- scalar(class_character, choices = c("user", "assistant"))

MCPAnnotations := new_class(
    properties = list(
        audience = optional(Role),
        priority = optional(scalar(class_numeric))
    )
)

MCPTextContent := new_class(
    properties = list(
        type = literal("text"),
        text = scalar(class_character),
        annotations = optional(MCPAnnotations)
    )
)

MCPImageContent := new_class(
    properties = list(
        type = literal("image"),
        data = scalar(class_character),
        mimeType = scalar(class_character),
        annotations = optional(MCPAnnotations)
    )
)

MCPAudioContent := new_class(
    properties = list(
        type = literal("audio"),
        data = scalar(class_character),
        mimeType = scalar(class_character),
        annotations = optional(MCPAnnotations)
    )
)

MCPResourceContents := new_class(
    properties = list(
        uri = scalar(class_character),
        mimeType = optional(scalar(class_character))
    )
)

MCPTextResourceContents := new_class(
    MCPResourceContents,
    properties = list(
        text = scalar(class_character)
    )
)

MCPBlobResourceContents := new_class(
    MCPResourceContents,
    properties = list(
        blob = scalar(class_character)
    )
)

MCPEmbeddedResource := new_class(
    properties = list(
        type = literal("resource"),
        resource = MCPTextResourceContents | MCPBlobResourceContents,
        annotations = optional(MCPAnnotations)
    )
)

MCPCallToolResult := new_class(
    MCPResult,
    properties = list(
        content = list_of(MCPTextContent | MCPImageContent | MCPAudioContent |
                              MCPEmbeddedResource),
        isError = optional(class_logical)
    )
)

MCPResource := new_class(
    properties = list(
        uri = scalar(class_character),
        name = scalar(class_character),
        description = optional(scalar(class_character)),
        mimeType = optional(scalar(class_character)),
        annotations = optional(MCPAnnotations),
        size = optional(scalar(class_integer))
    )
)

MCPListResourcesRequest := new_class(MCPRequest)

MCPListResourcesResult := new_class(
    MCPResult,
    properties = list(
        resources = list_of(MCPResource)
    )
)

MCPReadResourceRequest := new_class(
    MCPRequest,
    properties = list(
        uri = scalar(class_character)
    )
)

MCPReadResourceResult := new_class(
    MCPResult,
    properties = list(
        contents = list_of(MCPTextResourceContents | MCPBlobResourceContents)
    )
)

MCPResourceTemplate := new_class(
    properties = list(
        uriTemplate = scalar(class_character),
        name = scalar(class_character),
        description = optional(scalar(class_character)),
        mimeType = optional(scalar(class_character)),
        annotations = optional(MCPAnnotations)
    )
)

MCPListResourceTemplatesRequest := new_class(MCPRequest)

MCPListResourceTemplatesResult := new_class(
    MCPResult,
    properties = list(
        resourceTemplates = list_of(MCPResourceTemplate)
    )
)

MCPListPromptsRequest := new_class(MCPRequest)

MCPPromptArgument := new_class(
    properties = list(
        name = scalar(class_character),
        description = optional(scalar(class_character)),
        required = optional(scalar(class_logical))
    )
)

MCPPrompt := new_class(
    properties = list(
        name = scalar(class_character),
        description = optional(scalar(class_character)),
        arguments = list_of(MCPPromptArgument)
    )
)

MCPListPromptsResult := new_class(
    MCPResult,
    properties = list(
        prompts = list_of(MCPPrompt)
    )
)

MCPGetPromptRequest := new_class(
    MCPRequest,
    properties = list(
        name = scalar(class_character),
        arguments = optional(named(list_of(scalar(class_character))))
    )
)

MCPPromptMessage := new_class(
    properties = list(
        role = Role,
        content = MCPTextContent | MCPImageContent | MCPAudioContent |
            MCPEmbeddedResource
    )
)

MCPGetPromptResult := new_class(
    MCPResult,
    properties = list(
        description = optional(scalar(class_character)),
        messages = list_of(MCPPromptMessage)
    )
)

MCPLoggingMessageNotification := new_class(
    MCPNotification,
    properties = list(
        level = scalar(class_character),
        logger = optional(scalar(class_character)),
        data = class_any
    )
)

close.MCPSession <- function(con, ...) {
    close(con$endpoint, ...)
}

method(json_rpc_method, MCPInitializeRequest) <- function(x) "initialize"

method(json_rpc_method, MCPListToolsRequest) <- function(x) "tools/list"

method(json_rpc_method, MCPCallToolRequest) <- function(x) "tools/call"

method(json_rpc_method, MCPListResourcesRequest) <- function(x) "resources/list"

method(json_rpc_method, MCPReadResourceRequest) <- function(x) "resources/read"

method(json_rpc_method, MCPListResourceTemplatesRequest) <- function(x)
    "resources/templates/list"

method(json_rpc_method, MCPListPromptsRequest) <- function(x) "prompts/list"

method(json_rpc_method, MCPGetPromptRequest) <- function(x) "prompts/get"

method(json_rpc_method, MCPNotification) <- function(x) {
    name <- tolower(sub(".*MCP(.*)Notification", "\\1", class(x)[1L]))
    paste0("notifications/", name)
}

method(json_rpc_params, MCPRequest) <- function(x) jsonify(x)

method(response_prototype, MCPRequest) <- function(x) {
    match.fun(sub("wizrd::", "", sub("Request", "Result", class(x)[1L])))()
}

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

method_to_mcp_notification_class <- list(
    message = MCPLoggingMessageNotification
)

mcp_notification_mapper <- function(method) {
    name <- sub("^notifications/", "", method)
    method_to_mcp_notification_class[[name]]
}

method(receive, list(class_any, MCPResult)) <- function(from, as, ...) {
    receive(from, JSONRPCResponse(), mcp_notification_mapper) |>
        receive(as, ...)
}

method(receive, list(JSONRPCResponse, MCPResult)) <- function(from, as, ...) {
    from@result |> receive(as, ...)
}

method(receive, list(MCPSession, class_any)) <- function(from, as, ...) {
    receive(from$endpoint, as, ...)
}

mcp_connect <- function(server) {
    session <- MCPSession(endpoint = json_rpc_endpoint(server))
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

mcp_exec_server <- function(command, args = list()) {
    assert_string(command)
    assert_character(args, any.missing = FALSE)
    require_ns("processx", "connect to stdio-based MCP servers")
    try_uv_pipex(command, args) %||% pipex(command, args)
}

mcp_test_server <- function(transport = c("stdio", "sse"), port) {
    transport <- match.arg(transport)
    args <- c("fastmcp", "run", 
              system.file("mcp", "server.py", package = "wizrd"),
              "--transport", transport)
    if (transport == "sse")
        args <- c(args, "--port", port)
    mcp_exec_server("uvx", args)
}

method(tools, MCPSession) <- function(x) {
    x$listTools()
}

resources <- function(x) {
    c(x$listResources(), x$listResourceTemplates())
}

prompts <- function(x) {
    x$listPrompts()
}

method(convert, list(MCPTool, Tool)) <- function(from, to, session) {
    signature <- convert(from@inputSchema, ToolSignature)
    args <- formals(signature@parameters)
    name <- from@name
    body <- substitute({
        session$callTool(NAME, ARGS)
    }, list(NAME = name,
            ARGS = as.call(c(quote(parameters), sapply(names(args), as.name)))))
    envir <- list2env(list(session = session, parameters = signature@parameters),
                      parent = topenv())
    FUN <- as.function(c(args, body), envir)
    Tool(FUN,
         name = name,
         description = from@description,
         signature = signature,
         param_descriptions = param_descriptions_from_schema(from@inputSchema))
}

method(textify, list(MCPCallToolResult, TextFormat)) <- function(x, format) {
    ans <- lapply(x@content, textify, format)
    if (length(ans) == 1L)
        ans <- ans[[1L]]
    ans
}

method(textify, list(MCPTextContent, PlainTextFormat)) <- function(x, format) {
    x@text
}

method(textify, list(MCPImageContent | MCPAudioContent, PlainTextFormat)) <-
    function(x, format) convert(x, MediaURI)

method(convert, list(MCPImageContent, MediaURI)) <- function(from, to) {
    ImageURI(data_uri(from@data), type = from@mimeType)
}

method(convert, list(MCPAudioContent, MediaURI)) <- function(from, to) {
    AudioURI(data_uri(from@data), type = from@mimeType)
}

method(textify, list(MCPTextResourceContents, PlainTextFormat)) <-
    function(x, format) {
        x@text
    }

method(textify, list(MCPBlobResourceContents, PlainTextFormat)) <-
    function(x, format) {
        convert(x, MediaURI)
    }

method(convert, list(MCPBlobResourceContents, MediaURI)) <- function(from, to) {
    MediaURI(from@blob, type = from@mimeType)
}

method(as.function, MCPResource) <- function(x, session, ...) {
    ans <- eval(substitute(function() {
        session$readResource(URI)
    }, list(URI = x@uri)))
    environment(ans) <- list2env(list(session = session), parent = topenv())
    ans
}

method(as.function, MCPResourceTemplate) <- function(x, session, ...) {
    params <- glue_params(x@uriTemplate)
    ans <- eval(substitute(function() {
        ## FIXME: glue() only handles level 1 URI templates
        args <- lapply(ARGS, \(a) URLencode(as.character(a), reserved = TRUE))
        uri <- glue(URI_TEMPLATE, .envir = list2env(args, parent = emptyenv()))
        session$readResource(uri)
    }, list(URI_TEMPLATE = x@uriTemplate,
            ARGS = as.call(c(quote(list), sapply(params, as.name))))))
    formals(ans)[params] <- alist(x=)
    environment(ans) <- list2env(list(session = session), parent = topenv())
    ans
}

method(as.function, MCPPrompt) <- function(x, session, ...) {
    args <- vapply(x@arguments, \(a) a@name, character(1L))
    args_call <- as.call(c(quote(list), sapply(args, as.name)))
    required <- vapply(x@arguments, \(a) a@required %||% FALSE, logical(1L))
    ans <- if (all(required)) {
        eval(substitute(function() {
            session$getPrompt(NAME, ARGS)
        }, list(NAME = x@name, ARGS = args_call)))
    } else {
        missings <- sapply(args[!required], \(a) call("missing", as.name(a)))
        missing_args_call <- as.call(c(quote(c), missings))
        eval(substitute(function() {
            missing_args <- MISSING_ARGS
            args <- ARGS
            args[names(which(missing_args))] <- NULL
            session$getPrompt(NAME, args)
        }, list(MISSING_ARGS = missing_args_call, NAME = x@name,
                ARGS = args_call)))
    }
    formals(ans)[args] <- ifelse(required, alist(x=), list(NULL))
    environment(ans) <- list2env(list(session = session), parent = topenv())
    ans
}

method(convert, list(MCPPromptMessage, ChatMessage)) <- function(from, to) {
    ChatMessage(role = from@role,
                object = from@content,
                content = textify(from@content))
}

method(handle_notification, MCPLoggingMessageNotification) <- function(x) {
    logger <- switch(x@level, debug = verbose_message, info =, notice = message,
                     warning = warning, error =, critical =, alert =,
                     emergency = stop)
    logger(x@data)
}
