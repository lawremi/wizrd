TextProtocol <- new_class("TextProtocol",
                          properties = list(
                              input = new_property(
                                  TextFormat,
                                  default = quote(PlainTextFormat())),
                              output = new_property(
                                  TextFormat,
                                  default = quote(PlainTextFormat()))
                          ))

method(print, TextProtocol) <- function(x, ...) {
    cat(S7_class_desc(class(x@input)),
        cli::symbol$arrow_right, cli::symbol$arrow_left,
        S7_class_desc(class(x@output)))
    cat("\n")
}
