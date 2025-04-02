next_id <- local({
    id <- 0L
    function() {
        if (id == .Machine$integer.max)
            id <- 0L
        id <<- id + 1L
        id
    }
})

union_id <- class_character | class_numeric | NULL

JSONRPCRequest := new_class(
    properties = list(
        jsonrpc = scalar(class_character),
        method = scalar(class_character),
        params = class_list,
        id = scalar(union_id, default = quote(next_id()))
    )
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
        result = class_any,
        error = JSONRPCError | NULL,
        id = scalar(union_id)
    )
)

method(jsonify, JSONRPCRequest) <- function(x) {
    lapply(props(x), jsonify)
}

send := new_generic(c("x", "to"))

receive := new_generic(c("from", "as"))

response_prototype := new_generic("x")

invoke <- function(x, endpoint) {
    send(x, endpoint) |> receive(response_prototype(x))
}

method(send, list(JSONRPCRequest, class_any)) <- function(x, to) {
    jsonify(x) |> toJSON() |> send(to)
}

S3_connection <- new_S3_class("connection")

method(send, list(class_character, S3_connection)) <- function(x, to) {
    writeLines(x, to)
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

method(send, list(class_character, WebSocket)) <- function(x, to) {
    to$send(x)
    to
}

method(send, list(class_any, BufferedWebSocket)) <- function(x, to) {
    send(x, to@webSocket)
}

method(receive, list(S3_connection, JSONRPCResponse)) <- function(from, as) {
    readLines(from, n = 1L) |> receive(as)
}

method(receive, list(BufferedWebSocket, JSONRPCResponse)) <- function(from, as) {
    receive(from@buffer, as)
}

method(receive, list(class_character, JSONRPCResponse)) <- function(from, as) {
    fromJSON(from) |> receive(as)
}

method(receive, list(class_list, S7_object)) <- function(from, as) {
    do.call(S7_class(as), from)
}

method(response_prototype, JSONRPCRequest) <- function(x) JSONRPCResponse()

json_rpc_method := new_generic("x")
json_rpc_params := new_generic("x")

method(convert, list(class_any, JSONRPCRequest)) <- function(from, to) {
    JSONRPCRequest(method = json_rpc_method(from),
                   params = json_rpc_params(from)) 
}
