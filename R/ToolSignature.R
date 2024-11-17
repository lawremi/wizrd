ToolSignature := new_class(
    properties = list(
        parameters = new_property(
            S7_class,
            setter = \(self, value) {
                if (is.list(value))
                    value <- new_class("parameters",
                                       properties = dodge_dots(value))
                self@parameters <- value
                self
            }
        ),
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
