IOBinding <- new_class("IOBinding",
                       properties = list(
                           input = TextFormat,
                           output = TextFormat
                       ))

method(str, IOBinding) <- function(object, ...) {
    cat(S7:::obj_desc(object@input),
        cli::symbol$arrow_right, cli::symbol$arrow_left,
        S7:::obj_desc(object@output))
    cat("\n")
}
