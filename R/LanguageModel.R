LanguageModel <- new_class("LanguageModel", abstract = TRUE,
                           properties = list(
                               name = new_string_property(nullable = TRUE),
                               system_prompt =
                                   new_string_property(nullable = TRUE),
                               tools = new_list_property(of = class_function),
                               output_converter = OutputConverter
                           ))

llm <- new_generic("llm", "x")

chat <- new_generic("chat", "object")

embed <- new_generic("embed", "object")

## Vector databases / indexing methods to support
## - RcppAnnoy
## - duckdb (VSS extension)
## - tiledb
## - elastic
## - RPostgres (PGVector extension)
## - rredis/redux
## - RSQLite (VSS extension)

method(predict, LanguageModel) <- function(object, input, ...) {
    last_output(chat(object, input, ...))
}

append_system_prompt <- function(x, prompt) {
    set_props(system_prompt = paste(c(x@system_prompt, prompt),
                                    collapse = "\n\n")) 
}

prepare_input <- function(model, input) {
    if (!is.list(input) || is.object(input))
        input <- list(input)
    input <- lapply(input, normalize_chat_message)
    format_prompt <- format_prompt(x, model@output_converter@format)
    model <- append_system_prompt(model, format_prompt)
    Chat(model, messages = c(system = model@system_prompt, input))
}
