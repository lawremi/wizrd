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

    next_input <- messages
    ChatPipeline(lapply(x, \(xi) {
        if (inherits(xi, Chat)) {
            cht <- chat(xi, next_input, ...)
            next_input <<- last_output(cht)
            cht
        } else {
            next_intput <<- predict(xi, next_input, env = env, ...)
            xi
        }
    }))
}

method(textify, list(class_any, ChatPipeline)) <- function(x, format) x
method(detextify, list(class_any, ChatPipeline)) <- function(x, format) x

c_ChatPipeline <- function(...) {
    ChatPipeline(unlist(list(...), recursive = FALSE))
}

method(c, ChatPipeline) <- c_ChatPipeline
method(c, LanguageModel) <- c_ChatPipeline
