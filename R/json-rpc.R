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
        id = optional(NULL)
    ), # See S7 PR#473 for why this is necessary
    constructor = function(.data = JSONRPCRequest(method = method, id = NULL,
                                                  ...),
                           method = "", ...) {
        new_object(.data)
    }
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

HTTPEndpoint := new_class(
    properties = list(
        req = S3_httr2_request,
        resp = nullable(S3_httr2_response),
        auth = nullable(OAuth)
    ),
    abstract = TRUE
)

PostSSEEndpoint := new_class(HTTPEndpoint)

StreamableHTTPEndpoint := new_class(
    HTTPEndpoint,
    properties = list(
        session_id = nullable(scalar(class_character))
    )
)

post_sse_endpoint <- function(url) {
    sse_resp <- httr2::request(url) |>
        httr2::req_perform_connection(blocking = FALSE)
    event <- sse_resp |> resp_await_sse()
    if (event$type == "endpoint") # could be relative or absolute
        post_req <- event$data |>
            httr2::url_parse(base_url = url) |>
            httr2::url_build() |>
            httr2::request()
    else stop("expected 'endpoint' event from ", url)
    PostSSEEndpoint(req = post_req, resp = sse_resp)
}

streamable_http_endpoint <- function(url) {
    StreamableHTTPEndpoint(req = url |> httr2::request())
}

json_rpc_endpoint <- function(server) {
    if (is.character(server)) {
        if (resembles_url(server, "http") ||
                resembles_url(server, "https")) {
            if (url_path_ends_with(server, "sse"))
                post_sse_endpoint(server)
            else streamable_http_endpoint(server)
        } else {
            server
        }
    } else {
        server
    }
}

send := new_generic(c("x", "to"))

receive := new_generic(c("from", "as"))

response_class := new_generic("x")

handle_notification := new_generic("x")

invoke := new_generic("x", function(x, endpoint, ...) S7_dispatch())

method(invoke, JSONRPCRequest) <- function(x, endpoint, notification_mapper,
                                           canceller) {
    send(x, endpoint) |>
        receive(JSONRPCResponse(id = x@id), notification_mapper, canceller)
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

method(close, Pipe) <- function(con, ...) {
    con@process$kill()
}

method(close, PostSSEEndpoint) <- function(con, ...) {
    close(con@resp)
}

method(close, StreamableHTTPEndpoint) <- function(con, ...) {
    con@req |>
        httr2::req_method("DELETE") |>
        httr2::req_headers("Mcp-Session-Id" = con@session_id) |>
        httr2::req_perform()
}

method(send, list(S3_httr2_request, PostSSEEndpoint)) <-
    function(x, to) {
        httr2::req_perform(x)
        to
    }

method(send, list(S3_httr2_request, StreamableHTTPEndpoint)) <-
    function(x, to) {
        to@resp <- x |> httr2::req_headers(
            "Mcp-Session-Id" = to@session_id,
            "Accept" = "application/json, text/event-stream"
        ) |> httr2::req_perform_connection(blocking = FALSE)
        to@session_id <- httr2::resp_header(to@resp, "Mcp-Session-Id") %||%
            to@session_id
        to
    }

method(send, list(class_character | class_json, HTTPEndpoint)) <-
    function(x, to) {
        verbose_message("SEND: ", x)
        req <- to@req |> httr2::req_body_raw(x, "application/json")
        if (!is.null(to@auth))
            req <- req |> req_oauth_with(to@auth)
        result <- tryCatch(req |> send(to), httr2_http_401 = identity)
        if (inherits(result, "httr2_http_401")) {
            if (is.null(to@auth)) {
                to@auth <- oauth_auth_code_for_response(result$resp)
                to <- send(x, to)
            } else {
                stop(cnd)
            }
        } else {
            to <- result
        }
        to
    }

method(receive, list(Pipe, class_any)) <- function(from, as, ...) {
    receive(from@stdout, as, ...)
}

read_json_rpc_response := new_generic("x")

method(read_json_rpc_response, union_connection) <- function(x) {
    read_lines(x, n = 1L)
}

method(read_json_rpc_response, S3_httr2_response) <- function(x) {
    ctype <- httr2::resp_content_type(x)
    if (ctype == "application/json") {
        lines <- character()
        while(!httr2::resp_stream_is_complete(x))
            lines <- c(lines, httr2::resp_stream_lines(x))
        paste0(lines, collapse = "\n")
    } else if (ctype == "text/event-stream") {
        event <- httr2::resp_stream_sse(x)
        event$data
    } else {
        stop("unsupported content type: ", ctype)
    }
}

method(receive, list(union_connection | S3_httr2_response, JSONRPCResponse)) <-
    function(from, as, notification_mapper, canceller) {
        ## could become a more general listener that runs asynchronously,
        ## but that would only make sense for an app with mutable state
        on.exit(canceller())
        if (getOption("wizrd_test_json_rpc_cancel", FALSE))
            canceller()
        while (TRUE) {
            response <- read_json_rpc_response(from)
            if (length(response) > 0L) {
                verbose_message("RECEIVE: ", response)
                response <- fromJSON(response, simplifyDataFrame = FALSE)
                if (is.null(response$id))
                    receive_notification(response, notification_mapper)
                else if (identical(response$id, as@id))
                    break
                else warning("Received unexpected response: ", response$id)
            }
            Sys.sleep(0.01)
        }
        on.exit(add = FALSE)
        receive(response, as)
    }

method(receive, list(class_character, JSONRPCResponse)) <- function(from, as,
                                                                    ...) {
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
    stop(if (is.null(error_class)) {
        simpleError(error_msg)
    } else {
        errorCondition(error_msg, class = error_class$class)
    })
}

method(receive, list(class_list, JSONRPCResponse)) <- function(from, as) {
    response <- receive(from, super(as, S7_object))
    handle_json_rpc_error(response@error)
    response
}

method(receive, list(HTTPEndpoint, class_any)) <- function(from, as, ...) {
    receive(from@resp, as, ...)
}

method(response_class, JSONRPCRequest) <- function(x) JSONRPCResponse

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
