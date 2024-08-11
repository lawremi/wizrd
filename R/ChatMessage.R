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

normalize_chat_message <- new_generic("normalize_chat_message", "x")

method(normalize_chat_message, ChatMessage) <- identity

method(normalize_chat_message, class_any) <- function(x) {
    ChatMessage(role = "user", content = normalize_chat_content, object = x)
}

normalize_chat_content <- new_generic("normalize_chat_content", "x")

method(normalize_chat_content, class_list) <- function(x) {
    lapply(x, normalize_chat_content)
}

method(normalize_chat_content, class_character) <- function(x) {
    paste(x, collapse = "\n")
}

nativeRaster <- new_S3_class("nativeRaster")
raster <- new_S3_class("raster")
union_raster <- new_union(nativeRaster, raster)

method(normalize_chat_content, union_raster) <- identity

method(normalize_chat_content, class_any) <- function(x) {
    capture.output(print(x))
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
