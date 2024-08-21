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
                                                          role = "user",
                                                          format = SerialFormat)
{
    ChatMessage(role = role, content = serialize(x, format), object = x)
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
    if (length(x@content > 0L)) {
        if (x@role == "assistant")
            cli_text(x@content)
        else boxx(strwrap(x@content, width = console_width() / 2L),
                  float = float)
    }
    for (tool_call in tool_calls) {
        boxx(strwrap(capture.output(print(tool_call))), float = float,
             header = "Tool call", border_style = "classic")
    }
    if (length(x@object) > 0L && !identical(x@object, x@content)) {
        boxx(capture.output(print(x@content, width = console_width() / 2L)),
             float = float, header = "Object", border_style = "classic")
    }
}
