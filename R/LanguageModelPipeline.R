LanguageModelPipeline := new_class(
    class_list,
    validator = \(self) {
        if (!all(sapply(self, inherits, LanguageModel)))
            "all elements must inherit from LanguageModel"
    }
)

method(print, LanguageModelPipeline) <- function(x, ...) {
    cat(S7:::obj_desc(x)); cat(" ")
    cat(paste(names(x) %||% sapply(x, prop, "name"), collapse = " |> "))
    cat("\n")
}

method(convert, list(LanguageModel, LanguageModelPipeline)) <- function(from, to)
{
    LanguageModelPipeline(list(from))
}

c_LanguageModelPipeline <- function(...) {
    pipelines <- lapply(list(...), convert, LanguageModelPipeline)
    LanguageModelPipeline(unlist(pipelines))
}

# separate registration due to S7 issue #510
method(c, LanguageModel) <- c_LanguageModelPipeline
method(c, LanguageModelPipeline) <- c_LanguageModelPipeline

method(predict, LanguageModelPipeline) <- method(predict, LanguageModel)

method(perform_chat, LanguageModelPipeline) <- function(x, messages,
                                                        stream_callback, ...)
{
    stopifnot(is.null(stream_callback))
    Reduce(predict, x, init = messages, ...)
}

method(textify, list(class_any, LanguageModelPipeline)) <-
    function(x, format) x

method(detextify, list(class_any, LanguageModelPipeline)) <-
    function(x, format) x
