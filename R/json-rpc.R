next_id <- local({
    id <- 0L
    function() {
        if (id == .Machine$integer.max)
            id <- 0L
        id <<- id + 1L
        id
    }
})

prop_id <- scalar(class_character | class_numeric | NULL,
                  default = quote(next_id()))

JSONRPCRequest := new_class(
    method = prop_string,
    params = class_any,
    id = prop_id 
)

JSONRPCError := new_class(
    code = prop_integer,
    message = prop_string,
    data = class_any
)

JSONRPCResponse := new_class(
    result = new_property(
        class_any,
        setter = function(self, value) {
            if (is.list(value))
                value <- do.call(S7_class(self@value), value)
            self@value <- value
            self
        }
    ),
    error = JSONRPCError | NULL,
    id = prop_id
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

S3_connection <- new_S3_class("connection")

method(send, list(class_character, S3_connection)) <- function(x, to) {
    writeLines(x, to)
    to
}

method(receive, list(S3_connection, class_any) <- function(from, as) {
    readLines(from) |> receive(as)
}

method(send, list(JSONRPCRequest, class_any)) <- function(x, to) {
    jsonify(x) |> toJSON() |> send(to)
}

method(receive, list(class_character, JSONRPCResponse)) <- function(from, as) {
    fromJSON(from) |> receive(as)
}

method(receive, list(class_list, JSONRPCResponse)) <- function(from, as) {
    do.call(S7_class(as), from)
}

method(response_prototype, JSONRPCRequest) <- function(x) {
    JSONRPCResponse(result = response_prototype(x@params))
}
