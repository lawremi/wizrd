ToolCall <- new_class("ToolCall",
                      properties = list(
                          id = nullable(prop_string),
                          tool_name = prop_string,
                          arguments = class_list
                      ))

method(as.character, ToolCall) <- function(x) {
    as.character(convert(x, class_call))
}

method(convert, list(ToolCall, class_call)) <- function(from, to) {
    do.call(call, c(from@tool_name, from@arguments), quote = TRUE)
}

method(print, ToolCall) <- function(x, width = getOption("width"), ...) {
    cli::ansi_strtrim(cat("<ToolCall> ", x@id, ":", sep = ""), width)
    print(as.character(x), width = width, ...)
}
