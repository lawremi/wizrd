LanguageModelBackend <- new_class("LanguageModelBackend", abstract = TRUE)

complete_chat <- new_generic("complete_chat", "x")

perform_embedding <- new_generic("perform_embedding", "x")
