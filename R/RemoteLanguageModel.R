RemoteLanguageModel <- new_class("RemoteLanguageModel", LanguageModel,
                                 properties = list(server = LanguageModelServer,
                                                   name = prop_string))

method(perform_chat, RemoteLanguageModel) <- function(x, chat,
                                                      stream_callback = NULL,
                                                      ...)
{
    perform_chat(x@server, x@name, chat@messages, x@tools, x@io,
                 set_props(x@params, ...), stream_callback)
}

method(print, RemoteLanguageModel) <- function(x, ...) {
    NextMethod()
    cat("@server: ")
    print(x@server)
}
