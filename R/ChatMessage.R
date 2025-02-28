ChatMessage <- new_class("ChatMessage",
                         properties = list(
                             role = new_string_property(
                                 choices = c("system", "user", "assistant",
                                             "tool")
                             ),
                             content = class_character | class_list,
                             object = class_any,
                             tool_calls = new_list_property(of = ToolCall),
                             participant = nullable(prop_string),
                             refusal = nullable(prop_string)
                         ))

method(convert, list(class_any, ChatMessage)) <- function(from, to,
                                                          role = "user")
{
    ChatMessage(role = role, object = from)
}

method(convert, list(ChatMessage, ChatMessage)) <- function(from, to) from

method(textify, list(ChatMessage, TextFormat)) <- function(x, format) {
    set_props(x, content = textify(x@object, format))
}

method(detextify, list(ChatMessage, TextFormat)) <- function(x, format) {
    if (is.null(x@refusal))
        x@object <- detextify(x@content, format)
    x
}

split_into_blocks <- function(x) {
    regex <- "(?s)(.*?)```(.*?)\n(.*?)```|(.*)$"
    m <- regmatches(x, gregexec(regex, x, perl = TRUE))[[1L]][-1L,,drop = FALSE]
    labels <- rbind("text", m[2L,], "text")
    Filter(nzchar, setNames(as.list(m[-2L,]), labels))
}

esc <- function(x) paste0("{", x, "}")

float_for_role <- function(role) {
    switch(role, assistant = "left", user = "right", "center")
}

print_message_content <- function(x) {
    float <- float_for_role(x@role)
    if (length(x@content) > 0L) {
        if (x@role == "assistant")
            cat(paste(strwrap_preserve(x@content), collapse = "\n"))
        else if (x@role %in% c("user", "system")) {
            border_style <- switch(x@role, user = "single", system = "double")
            str <- strfit(x@content, width = cli::console_width() / 2L)
            cat(cli::boxx(str, float = float, border_style = border_style,
                          header = x@participant %||% ""))
            cat("\n")
        }
        cat("\n")
    }
}

print_message_object <- function(x) {
    float <- float_for_role(x@role)
    if (length(x@object) > 0L &&
            (!identical(x@object, x@content) || x@role == "tool")) {
        opts <- options(max.print = 10L)
        on.exit(options(opts))
        w <- cli::console_width() / 2L
        if (is.character(x@object))
            x@object <- strfit(x@object, width = w)
        str <- capture.output(print(x@object, width = w))
        cat(cli::boxx(str, float = float, header = x@participant %||% "",
                      border_style = "classic"))
        cat("\n")
    }
}

print_message_tool_calls <- function(x) {
    for (tool_call in x@tool_calls) {
        str <- capture.output(print(tool_call,
                                    width = cli::console_width() / 2L))
        cat(cli::boxx(str, float = "center", header = "Tool call",
                      border_style = "classic"))
    }
}

method(print, ChatMessage) <- function(x, ...) {
    if (x@role == "assistant") {
        print_message_tool_calls(x)
        print_message_content(x)
        print_message_object(x)
    } else {
        print_message_object(x)
        print_message_content(x)
    }
}
