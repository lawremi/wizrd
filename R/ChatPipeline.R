ChatPipeline := new_class(
    class_list,
    validator = \(self) {
        if (!all(sapply(self, S7:::class_inherits, Chat | LanguageModel)))
            paste("all elements must inherit from Chat or LanguageModel")
    }
)

method(print, ChatPipeline) <- function(x, ...) {
    cat(S7:::obj_desc(x)); cat(" ")
    cat(paste(names(x) %||% seq_along(x), collapse = " |> "))
    cat("\n")
}

method(predict, ChatPipeline) <- predict_via_chat

method(chat, ChatPipeline) <- function(x, input = NULL,
                                       stream_callback = NULL,
                                       ..., env = parent.frame())
{
    chat(Chat(model = x, env = env), input, stream_callback, ...)
}

method(perform_chat, ChatPipeline) <- function(x, messages, stream_callback, env,
                                               ...)
{
    stopifnot(is.null(stream_callback))

    next_input <- messages[[length(messages)]]
    messages <- head(messages, -1L)
    last_message <- NULL
    pipeline <- ChatPipeline(lapply(x, \(xi) {
        if (!inherits(xi, Chat))
            cht <- convert(xi, Chat, messages = messages, env = env)
        else cht <- xi
        cht <- chat(cht, next_input, ...)
        last_message <<- last_message(cht)
        next_input <<- last_message@object
        if (inherits(xi, Chat))
            cht
        else xi
    }))
    
    Chat(model = pipeline, messages = c(messages, last_message))
}

method(textify, list(class_any, ChatPipeline)) <- function(x, format) {
    if (length(format) > 0L)
        textify(x, format[[1L]])
}

method(detextify, list(class_any, ChatPipeline)) <- function(x, format) x

c_ChatPipeline <- function(...) {
    ChatPipeline(unlist(list(...), recursive = FALSE))
}

method(c, ChatPipeline) <- c_ChatPipeline
method(c, LanguageModel) <- c_ChatPipeline
