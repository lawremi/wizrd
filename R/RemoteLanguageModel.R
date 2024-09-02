RemoteLanguageModel <- new_class("RemoteLanguageModel", LanguageModel,
                                 properties = list(server = LanguageModelServer,
                                                   name = prop_string))

method(chat, RemoteLanguageModel) <- function(x, input,
                                              stream_callback = NULL, ...)
{
    chat <- prepare_input(x, input)
  
    output <- chat(x@server, x@name, chat@messages,
                   x@tools, x@io, set_props(x@params, ...),
                   stream_callback)

    handle_output(chat, output)
}

method(print, RemoteLanguageModel) <- function(x, ...) {
    NextMethod()
    cat("@server: ")
    print(x@server)
}
