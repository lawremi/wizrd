ChatMessage <- new_class("ChatMessage",
                         properties = list(
                             role = new_string_property(
                                 choices = c("system", "user", "assistant",
                                             "tool")
                             ),
                             content = class_any,
                             object = class_any,
                             tool_calls = new_list_property(of = ToolCall),
                             participant = nullable(prop_string)
                         ))

method(convert, list(class_any, ChatMessage)) <- function(from, to,
                                                          role = "user")
{
    ChatMessage(role = role, object = from)
}

method(textify, list(ChatMessage, TextFormat)) <- function(x, format) {
    set_props(x, content = textify(x@object, format))
}

method(detextify, list(ChatMessage, TextFormat)) <- function(x, format) {
    x@object <- detextify(x@content, format)
    x
}

split_into_blocks <- function(x) {
    regex <- "(?s)(.*?)```(.*?)\n(.*?)```|(.*)$"
    m <- regmatches(x, gregexec(regex, x, perl = TRUE))[[1L]][-1L,,drop = FALSE]
    labels <- rbind("text", m[2L,], "text")
    Filter(nzchar, setNames(as.list(m[-2L,]), labels))
}

method(print, ChatMessage) <- function(x, ...)
{
    float <- switch(x@role, assistant = "left", user = "right", "center")
    if (length(x@content) > 0L) {
        if (x@role == "assistant")
            cli::cli_text(x@content)
        else if (x@role %in% c("user", "system")) {
            border_style <- switch(x@role, user = "single", system = "double")
            cat(cli::boxx(strwrap(x@content,
                                  width = cli::console_width() / 2L),
                          float = float, border_style = border_style,
                          header = x@participant %||% ""))
            cat("\n")
        }
    }
    for (tool_call in x@tool_calls) {
        cat(cli::boxx(strwrap(capture.output(print(tool_call))), float = float,
                      header = "Tool call", border_style = "classic"))
    }
    if (length(x@object) > 0L &&
            !identical(x@object, x@content)) {
        cat(cli::boxx(capture.output(print(x@object,
                                           width = cli::console_width() / 2L)),
                      float = float, header = x@participant %||% "",
                      border_style = "classic"))
    }
}
