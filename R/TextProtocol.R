TextProtocol <- new_class("TextProtocol",
                          properties = list(
                              input = new_property(
                                  TextFormat,
                                  default = PlainTextFormat()),
                              output = new_property(
                                  TextFormat,
                                  default = PlainTextFormat())
                          ))

method(print, TextProtocol) <- function(x, ...) {
    cat(S7:::obj_desc(x@input),
        cli::symbol$arrow_right, cli::symbol$arrow_left,
        S7:::obj_desc(x@output))
    cat("\n")
}
