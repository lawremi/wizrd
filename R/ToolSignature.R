ToolSignature <- new_class("ToolSignature",
                           properties = list(
                               parameters = S7_class,
                               value = union_classes
                           ))

method(print, ToolSignature) <- function(x, ...) {
    classes <- vapply(props(x@parameters), S7:::class_desc, character(1L))
    cat("(", paste0(names(props(x@parameters)), ": ", classes, collapse = ", "),
        "): ", S7:::class_desc(x@value), "\n", sep = "")
}

tool_signature <- function(`_value` = class_any, ...) {
    parameters <- new_class("parameters", properties = list(...)) 
    ToolSignature(value = `_value`, parameters = parameters)
}
