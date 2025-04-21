ToolCall <- new_class("ToolCall",
                      properties = list(
                          id = nullable(scalar(class_character)),
                          tool_name = scalar(class_character),
                          arguments = class_list
                      ))

method(as.character, ToolCall) <- function(x) {
    deparse(convert(x, class_call))
}

method(convert, list(ToolCall, class_call)) <- function(from, to) {
    as.call(c(as.name(from@tool_name), from@arguments))
}

method(print, ToolCall) <- function(x, width = getOption("width"), ...) {
    cat(cli::ansi_strtrim(paste0("<ToolCall> ", x@id, ": "), width))
    print(as.character(x), width = width, ...)
}
