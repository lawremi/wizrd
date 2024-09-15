ToolSignature <- new_class("ToolSignature",
                           properties = list(
                               parameters = S7_class,
                               value = union_classes
                           ))

method(print, ToolSignature) <- function(x, ...) {
    classes <- vapply(x@parameters@properties, \(p) S7:::class_desc(p$class),
                      character(1L))
    cat("(", paste0(names(x@parameters@properties), ": ",
                    classes, collapse = ", "),
        "): ", S7:::class_desc(x@value), "\n", sep = "")
}

dodge_dots <- function(x) {
    names(x)[names(x) == "..."] <- "_dots"
    x
}

tool_signature <- function(`_value` = class_any, `_parameters` = list(...), ...)
{
    parameters <- new_class("parameters", properties = dodge_dots(`_parameters`))
    ToolSignature(value = `_value`, parameters = parameters)
}
