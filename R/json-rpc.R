JSON_RPC_VERSION <- "2.0"

next_id <- local({
    id <- 0L
    function() {
        if (id == .Machine$integer.max)
            id <- 0L
        id <<- id + 1L
        id
    }
})

union_id <- NULL | class_character | class_numeric

JSONRPCRequest := new_class(
    properties = list(
        jsonrpc = literal(JSON_RPC_VERSION),
        method = scalar(class_character),
        params = class_list,
        id = scalar(union_id, default = quote(next_id()))
    )
)

JSONRPCNotification := new_class(
    JSONRPCRequest,
    properties = list(
        id = NULL
    ), # See S7 PR#473 for why this is necessary
    constructor = function(.data = JSONRPCRequest(method = method, id = NULL,
                                                  ...),
                           method = "", ...)
        new_object(.data)
)

JSONRPCError := new_class(
    properties = list(
        code = scalar(class_integer),
        message = scalar(class_character),
        data = class_any
    )
)

JSONRPCResponse := new_class(
    properties = list(
        result = optional(class_any),
        error = optional(JSONRPCError),
        id = scalar(union_id)
    )
)

S3_httr2_response <- new_S3_class("httr2_response")
S3_httr2_request <- new_S3_class("httr2_request")

PostSSEEndpoint := new_class(
    properties = list(
        post_req = S3_httr2_request,
        sse_resp = S3_httr2_response
    )
)

ws_endpoint <- function(url) {
    require_ns("websocket", "connect to websocket-based MCP servers")
    BufferedWebSocket(webSocket = WebSocket$new(url))
}

post_sse_endpoint <- function(url) {
    sse_resp <- httr2::request(url) |>
        httr2::req_perform_connection(blocking = FALSE)
    event <- sse_resp |> resp_await_sse()
    if (event$type == "endpoint") # could be relative or absolute
        post_req <- event$data |> httr2::url_parse(base_url = url) |>
            httr2::url_build() |> httr2::request()
    else stop("expected 'endpoint' event from ", url)
    PostSSEEndpoint(post_req = post_req, sse_resp = sse_resp)
}

json_rpc_endpoint <- function(server) {
    if (is.character(server)) {
        if (resembles_url(server, "ws") || resembles_url(server, "wss"))
            ws_endpoint(server)
        else if (resembles_url(server, "http") || resembles_url(server, "https"))
            post_sse_endpoint(server)
        else server
    } else server
}

send := new_generic(c("x", "to"))

receive := new_generic(c("from", "as"))

response_prototype := new_generic("x")

handle_notification := new_generic("x")

invoke <- function(x, endpoint) {
    send(x, endpoint) |> receive(response_prototype(x))
}

notify <- function(x, endpoint) {
    send(x, endpoint)
    invisible(NULL)
}

method(send, list(JSONRPCRequest, class_any)) <- function(x, to) {
    jsonify(x) |> toJSON(null = "null") |> send(to)
    to
}

method(send, list(class_any, Pipe)) <- function(x, to) {
    send(x, to@stdin)
    to
}

method(send, list(class_character | class_json, union_connection)) <-
    function(x, to) {
        verbose_message("SEND: ", x)
        write_lines(to, x)
        to
    }

WebSocket <- new_S3_class("WebSocket") # R6 class from the websocket package

BufferedWebSocket := new_class(
    properties = list(
        buffer = new_property(
            S3_connection,
            setter = function(self, value) {
                if (is.null(self@buffer))
                    self@buffer <- value
                else stop("@buffer is read only after construction")
                self
            },
            default = quote(file())
        ),
        webSocket = new_property(
            WebSocket,
            setter = function(self, value) {
                value$onMessage(function(event) {
                    writeLines(event$data, self@buffer)
                })
                self@webSocket <- value
                self
            }
        )
    )
)

method(close, BufferedWebSocket) <- function(con, ...) {
    con@webSocket$close()
    close(con@buffer, ...)
}

method(close, Pipe) <- function(con, ...) {
    con@process$kill()
}

method(send, list(class_character | class_json, WebSocket)) <- function(x, to) {
    to$send(x)
    to
}

method(send, list(class_any, BufferedWebSocket)) <- function(x, to) {
    send(x, to@webSocket)
    to
}

method(receive, list(Pipe, class_any)) <- function(from, as, ...) {
    receive(from@stdout, as, ...)
}

method(receive, list(union_connection, JSONRPCResponse)) <-
    function(from, as, on_notify = function(x) invisible(NULL))
    {
        ## could become a more general listener that runs asynchronously,
        ## but that would only make sense for an app with mutable state
        while(TRUE) {
            lines <- read_lines(from, n = 1L)
            if (length(lines) >= 1L) {
                if (getOption("wizrd_verbose", FALSE))
                    message("RECEIVE: ", lines)
                response <- fromJSON(lines, simplifyDataFrame = FALSE)
                if (is.null(response$id))
                    receive(from, JSONRPCNotification()) |> on_notify()
                else break
            }
            Sys.sleep(0.01)
        }
        receive(response, as)
    }

method(receive, list(BufferedWebSocket, JSONRPCResponse)) <- function(from, as,
                                                                      ...)
{
    receive(from@buffer, as, ...)
}

method(receive, list(class_character, JSONRPCResponse)) <- function(from, as,
                                                                    ...)
{
    fromJSON(from) |> receive(as, ...)
}

method(receive, list(class_list, S7_object)) <- function(from, as) {
    dejsonify(from, S7_class(as))
}

receive_notification <- function(from, mapper) {
    notification <- receive(from, JSONRPCNotification())
    dejsonify(notification@params, mapper(notification@method)) |>
        handle_notification()
}

handle_json_rpc_error <- function(e) {
    if (is.null(e))
        return(invisible(NULL))
    error_msg <- paste("MCP Error:", e@message)
    error_class <- JSON_RPC_ERRORS[[as.character(e@code)]]
    if (is.null(error_class)) {
        simpleError(error_msg)
    } else {
        errorCondition(error_msg, class = error_class$class)
    } |> stop()
}

method(receive, list(class_list, JSONRPCResponse)) <- function(from, as) {
    response <- receive(from, super(as, S7_object))
    handle_json_rpc_error(response@error)
    response
}

method(response_prototype, JSONRPCRequest) <- function(x) JSONRPCResponse()

json_rpc_method := new_generic("x")
json_rpc_params := new_generic("x")

method(convert, list(class_any, JSONRPCRequest | JSONRPCNotification)) <-
    function(from, to) {
        to(method = json_rpc_method(from),
           params = json_rpc_params(from)) 
    }

new_error_class <- function(x) new_S3_class(c(x, "error", "condition"))

S3_JSONRPCParseError <- new_error_class("JSONRPCParseError")
S3_JSONRPCInvalidRequest <- new_error_class("JSONRPCInvalidRequest")
S3_JSONRPCMethodNotFound <- new_error_class("JSONRPCMethodNotFound")
S3_JSONRPCInvalidParams <- new_error_class("JSONRPCInvalidParams")
S3_JSONRPCInternalError <- new_error_class("JSONRPCInternalError")

JSON_RPC_ERRORS <- list(
    "-32700" = S3_JSONRPCParseError,
    "-32600" = S3_JSONRPCInvalidRequest,
    "-32601" = S3_JSONRPCMethodNotFound,
    "-32602" = S3_JSONRPCInvalidParams,
    "-32603" = S3_JSONRPCInternalError
)
