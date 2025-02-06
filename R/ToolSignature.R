Parameters := new_class(abstract = TRUE)

new_parameters <- function(x = list()) {
    new_class("parameters", Parameters, properties = dodge_dots(x))
}

ToolSignature := new_class(
    properties = list(
        parameters = new_property(
            S7_class,
            setter = \(self, value) {
                if (is.list(value))
                    value <- new_parameters(value)
                self@parameters <- value
                self
            },
            default = quote(new_parameters())
        ),
        value = S7_property
    ))

method(print, ToolSignature) <- function(x, ...) {
    classes <- vapply(x@parameters@properties, \(p) S7:::class_desc(p$class),
                      character(1L))
    cat("(", paste0(names(x@parameters@properties), ": ",
                    classes, collapse = ", "),
        "): ", S7:::class_desc(x@value$class), "\n", sep = "")
}

dodge_dots <- function(x) {
    if (!is.null(x))
        names(x)[names(x) == "..."] <- "_dots"
    x
}

Parameters_as_call <- function(x) {
    as.call(c(quote(alist), expand_dots(props(x))))
}

## Does not help printing from inside a data.frame, because
## format.default() delegates specifically to format.default() when
## looping over a list (column).
## It should probably call the generic instead.
method(format, Parameters) <- function(x, ...) {
    format(Parameters_as_call(x), ...)
}

method(print, Parameters) <- function(x, ...) {
    print(Parameters_as_call(x), ...)
}
