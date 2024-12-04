LanguageModelPipeline := new_class(
    properties = list(
        models = new_list_property(of = LanguageModel)
    )
)

method(perform_chat, LanguageModelPipeline) <- function(x, messages,
                                                        stream_callback, ...)
{
    stopifnot(is.null(stream_callback))
    Reduce(predict, x@models, init = messages, ...)
}

method(talks_to, LanguageModelPipeline) <- function(x, target) {
    x@models <- c(x@models, target)
    x
}

method(textify, list(class_any, LanguageModelPipeline)) <-
    function(x, format) x

method(detextify, list(class_any, LanguageModelPipeline)) <-
    function(x, format) x
