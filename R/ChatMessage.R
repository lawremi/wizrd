ChatMessage <- new_class("ChatMessage",
                         properties = list(
                             role = new_string_property(
                                 choices = c("system", "user", "assistant",
                                             "tool")
                             ),
                             content = class_any,
                             object = class_any,
                             tool_calls = new_list_property(of = ToolCall)
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

method(str, ChatMessage) <- function(object, ...)
{
    float <- switch(object@role, assistant = "left", user = "right", "center")
    border_style <- switch(object@role, user = "single", system = "double")
    if (length(object@content) > 0L) {
        if (object@role == "assistant")
            cli::cli_text(object@content)
        else {
            cat(cli::boxx(strwrap(object@content,
                                  width = cli::console_width() / 2L),
                          float = float, border_style = border_style))
            cat("\n")
        }
    }
    for (tool_call in object@tool_calls) {
        cat(cli::boxx(strwrap(capture.output(print(tool_call))), float = float,
                      header = "Tool call", border_style = "classic"))
    }
    if (length(object@object) > 0L &&
            !identical(object@object, object@content)) {
        cat(cli::boxx(capture.output(print(object@content,
                                           width = cli::console_width() / 2L)),
                      float = float, header = "Object",
                      border_style = "classic"))
    }
}
