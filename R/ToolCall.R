ToolCall <- new_class("ToolCall",
                      properties = list(
                          id = prop_string,
                          tool_name = prop_string,
                          arguments = class_list
                      ))

method(print, ToolCall) <- function(x, ...) {
    cat("<ToolCall> ", x@id, ":", sep = "")
    print(do.call(call, c(x@tool_name, x@arguments), quote = TRUE))
}
