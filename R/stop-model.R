stop_model <- new_generic("stop_model", "x")

method(stop_model, LanguageModelBackend) <- function(x) {
    invisible(FALSE)
}

method(stop_model, LanguageModelServer) <- function(x) {
    process <- x@process
    if (is.null(process) || !process$is_alive())
        return(invisible(FALSE))

    process$kill()
    invisible(TRUE)
}

method(stop_model, Agent) <- function(x) {
    stop_model(x@backend)
}

method(stop_model, ChatPipeline) <- function(x) {
    stopped <- vapply(x, stop_model, logical(1L))
    invisible(any(stopped))
}

method(stop_model, Chat) <- function(x) {
    stop_model(x@model)
}
