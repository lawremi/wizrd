LanguageModelBackend <- new_class("LanguageModelBackend", abstract = TRUE)

perform_chat <- new_generic("perform_chat", "x")

perform_embedding <- new_generic("perform_embedding", "x")
