LanguageModel <- new_class("LanguageModel", abstract = TRUE,
                           properties = list(
                               name = prop_string_nullable,
                               instructions = new_string_property(
                                   default = "You are a helpful assistant."
                               ),
                               binding = FormatBinding,
                               tools = new_list_property(of = BoundTool)
                           ))

language_model <- new_generic("language_model", "x")

chat <- new_generic("chat", "x")

embed <- new_generic("embed", "x")

## Vector databases / indexing methods to support
## - RcppAnnoy
## - duckdb (VSS extension)
## - tiledb
## - RSQLite (VSS extension)
## - rsolr (KnnQParser)
## - elastic
## - RPostgres (PGVector extension)
## - rredis/redux

method(predict, LanguageModel) <- function(object, input, ...) {
    last_output(chat(object, input, ...))
}

compile_instructions <- new_generic("compile_instructions", "x")

method(compile_instructions, LanguageModel) <- function(x) {
    paste(c(x@instructions,
            instructions(x@binding, x),
            tool_instructions(x),
            collapse = "\n\n"))
}

prepare_input <- function(model, input) {
    if (!is.list(input) || is.object(input))
        input <- list(input)
    input <- lapply(input, convert, ChatMessage, format = model@binding@input)
    instructions <- compile_instructions(model)
    input$system <- NULL
    Chat(model, messages = c(system = instructions, input))
}

tool_instructions <- new_generic("tool_instructions", "x")

method(tool_instructions, LanguageModel) <- function(x) {
    if (length(x@tools) == 0L)
        return(NULL)
    
    paste0("You have access to one or more tools named ",
           paste(names(x@tools), collapse = ", "),
           ". Use them when relevant to answer user queries, but ",
           "prioritize generating responses without them unless ",
           "the user explicitly requests or ",
           "the task requires precise computation or external data.\n\n",
           paste(unlist(lapply(x@tools, \(tool) {
               if (!is.null(tool@instructions))
                   paste0("Specific instructions for '", tool@name, "':\n",
                          tool@instructions)
           })), collapse = "\n\n"))
}
