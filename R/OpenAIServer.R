OpenAIServer <- new_class("OpenAIServer", OpenAIAPIServer,
                          properties = list(
                              url = new_property(class_character,
                                                 default =
                                                     "https://api.openai.com"),
                              key_prefix = new_property(class_character,
                                                        default = "OPENAI")
                          ))
