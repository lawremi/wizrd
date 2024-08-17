RemoteLanguageModel <- new_class("RemoteLanguageModel", LanguageModel,
                                 properties = list(server = LanguageModelServer,
                                                   name = prop_string))

method(chat, RemoteLanguageModel) <- function(x, input,
                                              stream_callback = NULL, ...)
{
    chat <- prepare_input(x, input)
  
    response <- chat(x@server, x@name, chat@messages,
                     x@tools, x@binding,
                     stream_callback, ...)

    handle_output(chat, chat_message(response))
}
