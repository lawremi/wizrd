LanguageModel <- new_class("LanguageModel", abstract = TRUE,
                           properties = list(
                               name = prop_string_nullable,
                               instructions = new_string_property(
                                   default = "You are a helpful assistant."
                               ),
                               io = IOBinding,
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

method(str, LanguageModel) <- function(object, ...) {
    cat(S7:::obj_desc(object))
    if (!is.null(object@name)) cat("", object@name)
    cat("\n")
    cat(cli::ansi_strtrim(paste("@instructions:", object@instructions)))
    cat("\n")
    cat("@io: ")
    str(object@io)
    tool_names <- vapply(object@tools, `@`, character(1L), "name")
    cat(cli::ansi_strtrim(paste("@tools:", paste(tool_names, collapse = ", "))))
    cat("\n")
}

method(predict, LanguageModel) <- function(object, input, ...) {
    last_output(chat(object, input, ...))
}

compile_instructions <- new_generic("compile_instructions", "x")

method(compile_instructions, LanguageModel) <- function(x) {
    paste(c(x@instructions,
            instructions(x@io, x),
            tool_instructions(x)),
          collapse = "\n\n")
}

instructions <- new_generic("instructions", c("on", "to"))

method(instructions, list(IOBinding, LanguageModel)) <- function(on, to) {
    instr <- c(input_instructions(on@input, to),
               output_instructions(on@output, to))
    if (!is.null(instr)) paste(instr, collapse = "\n\n")
}

prepare_input <- function(model, input) {
    if (!is.list(input) || is.object(input))
        input <- list(input)
    input <- lapply(input, convert, ChatMessage)
    input <- lapply(input, textify, format = model@io@input)
    instructions <- compile_instructions(model)
    system <- ChatMessage(role = "system",
                          content = textify(instructions, TextFormat()))
    input$system <- NULL
    Chat(model, messages = c(system = system, input))
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

input_instructions <- new_generic("input_instructions", c("on", "to"))

output_instructions <- new_generic("output_instructions", c("on", "to"))

method(input_instructions,
       list(TextFormat, LanguageModel)) <- function(on, to)
{
    NULL
}

method(output_instructions,
       list(TextFormat, LanguageModel)) <- function(on, to)
{
    NULL
}

method(output_instructions,
       list(JSONFormat, LanguageModel)) <- function(on, to)
{
    prompt <- "Return only JSON, without any explanation or other text.\n"
    if (length(on@schema) > 0L)
        prompt <- paste0(prompt,
                         "The JSON must conform to the following schema:\n\n",
                         toJSON(on),
                         "\nEnsure the JSON matches this schema exactly.\n")
    if (length(on@example) > 0L)
        prompt <- paste(prompt, "Here is an example:\n\n", toJSON(on@example))
    prompt
}

method(input_instructions,
       list(JSONFormat, LanguageModel)) <- function(on, to)
{
    prompt <- "The user will send only JSON, without any other text.\n"
    if (length(on@schema) > 0L)
        prompt <- paste0(prompt,
                         "The JSON will conform to the following schema:\n\n",
                         toJSON(on),
                         "\nExpect the JSON to match this schema exactly.\n")
    if (length(on@example) > 0L)
        prompt <- paste(prompt, "Here is an example:\n\n", toJSON(on@example))
    prompt
}

method(output_instructions,
       list(CSVFormat, LanguageModel)) <- function(on, to)
{
    prompt <- "Return only CSV, without any explanation or other text.\n"
    if (length(on@schema) > 0L)
        prompt <- paste0(prompt,
                         "This JSON schema defines the structure of the data:\n",
                         toJSON(on),
                         "\nInterpret the JSON schema to understand ",
                         "the required columns and data types and produce ",
                         "the corresponding CSV. ",
                         "Ensure the CSV matches this schema exactly.\n")
    if (length(on@example) > 0L)
        prompt <- paste(prompt, "Here is an example:\n\n", toJSON(on@example))
    prompt
}

method(input_instructions,
       list(CSVFormat, LanguageModel)) <- function(on, to)
{
    prompt <- "The user will send only CSV, without any other text.\n"
    if (length(on@schema) > 0L)
        prompt <- paste0(prompt,
                         "This JSON schema defines the structure of the data:\n",
                         toJSON(on),
                         "\nInterpret the JSON schema to understand ",
                         "the expected columns and data types. ",
                         "Expect the CSV to match this schema exactly.\n")
    if (length(on@example) > 0L)
        prompt <- paste(prompt, "Here is an example:\n\n", toJSON(on@example))
    prompt
}

markdown_code_example <- function(format) {
    paste(c(paste(c("```", format@language), collapse = ""),
            deparse(format@example), "```\n"), collapse = "\n")
}

method(output_instructions,
       list(CodeFormat, LanguageModel)) <- function(on, to)
{
    prompt <- paste(c("Return only", on@language,
                      "code in markdown-style blocks,",
                      "without any explanation or other text.\n"),
                    collapse = " ")
    if (length(on@example) > 0L)
        prompt <- paste(prompt, "Here is an example:\n",
                        markdown_code_example(on))
    prompt
}

method(input_instructions,
       list(CodeFormat, LanguageModel)) <- function(on, to)
{
    prompt <- paste(c("The user will send only", on@language,
                      "code in markdown-style blocks,",
                      "without any explanation or other text.\n"),
                    collapse = " ")
    if (length(on@example) > 0L)
        prompt <- paste(prompt, "Here is an example:\n",
                        markdown_code_example(on))
    prompt
}

tool_input_format <- new_generic("tool_input_format", "x",
                                 function(x, tool, ...) S7_dispatch())

method(tool_input_format, LanguageModel) <- function(x, tool) {
    tool_input_json_format(x, tool)
}

tool_output_format <- new_generic("tool_output_format", "x",
                                  function(x, tool, ...) S7_dispatch())

method(tool_output_format, LanguageModel) <- function(x, tool) {
    tool_output_json_format(x, tool)
}

bind_fun <- function(FUN) {
    function(args) {
        do.call(FUN, args)
    }
}

bind <- new_generic("bind", c("x", "to"))

method(bind, list(Tool, LanguageModel)) <- function(x, to, instructions = NULL) {
    assert_string(instructions, null.ok = TRUE)
    io_binding <- IOBinding(input = tool_input_format(to, x),
                            output = tool_output_format(to, x))
    do.call(BoundTool, c(bind_fun(x), props(x), io = io_binding,
                         instructions = instructions))
}

method(bind, list(BoundTool, LanguageModel)) <- function(x, to) x
