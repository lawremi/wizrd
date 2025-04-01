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
    jsonrpc = scalar(class_character),
    method = scalar(class_character),
    params = class_list,
    id = scalar(union_id, default = quote(next_id()))
)

JSONRPCError := new_class(
    code = scalar(class_integer),
    message = scalar(class_character),
    data = class_any
)

JSONRPCResponse := new_class(
    result = class_any,
    error = JSONRPCError | NULL,
    id = scalar(union_id)
)

method(jsonify, JSONRPCRequest) <- function(x) {
    lapply(props(x), jsonify)
}

send := new_generic(c("x", "to"))

receive := new_generic(c("from", "as"))

response_prototype := new_generic()

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
            setter = function(self, value) stop("@buffer is not settable"),
            default = quote(file())
        ),
        webSocket = new_property(
            WebSocket,
            setter = function(self, value) {
                value$onMessage(function(event) {
                    writeLines(self@buffer, event$data)
                })
                self@value <- value
                self
            }
        )
    )
)

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

json_rpc_method := new_generic()
json_rpc_params := new_generic()

method(convert, list(class_any, JSONRPCRequest)) <- function(from, to) {
    JSONRPCRequest(method = json_rpc_method(from),
                   params = json_rpc_params(from)) 
}
