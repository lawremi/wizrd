RemoteLanguageModel <- new_class("RemoteLanguageModel", LanguageModel,
                                 properties = list(server = LanguageModelServer,
                                                   name = prop_string))

method(chat, RemoteLanguageModel) <- function(object, input,
                                              stream_callback = NULL, ...)
{
    chat <- prepare_input(object, input)
  
    response <- chat(object@server, object@name, chat@messages,
                     object@tools, object@output_converter@format,
                     stream_callback, ...)

    handle_output(chat, chat_messages(response))
}
