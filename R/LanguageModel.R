LanguageModel <- new_class("LanguageModel", abstract = TRUE,
                           properties = list(
                               name = nullable(prop_string),
                               instructions = new_string_property(
                                   default = "You are a helpful assistant."
                               ),
                               io = TextProtocol,
                               tools = new_list_property(of = ToolBinding),
                               params = LanguageModelParams
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

method(print, LanguageModel) <- function(x, ...) {
    cat(S7:::obj_desc(x))
    if (!is.null(x@name)) cat("", x@name)
    cat("\n")
    cat(cli::ansi_strtrim(paste("@instructions:", x@instructions)))
    cat("\n")
    cat("@io: ")
    print(x@io)
    tool_names <- vapply(x@tools, \(binding) binding@tool@name, character(1L))
    cat(cli::ansi_strtrim(paste("@tools:", paste(tool_names, collapse = ", "))))
    cat("\n")
    params <- unlist(props(x@params))
    param_str <- paste(names(params), rep("=", length(params)), params,
                       collapse = ", ")
    cat(cli::ansi_strtrim(paste("@params:", param_str)))
    cat("\n")
}

method(predict, LanguageModel) <- function(object, input, env = parent.frame(),
                                           ...)
{
    last_output(chat(object, input, env = env, ...))
}

instruct <- function(x, ...) {
    set_props(x, instructions = paste0(...))
}

compile_instructions <- new_generic("compile_instructions", "x")

method(compile_instructions, LanguageModel) <- function(x) {
    paste(c(tool_instructions(x),
            instructions(x@io@output, x),
            x@instructions),
          collapse = "\n\n")
}

instructions <- new_generic("instructions", c("on", "to"))

prepare_input <- function(model, input, env) {
    if (!is.list(input) || is.object(input))
        input <- list(input)
    input <- lapply(input, convert, ChatMessage)
    input <- lapply(input, textify, format = model@io@input)
    instructions <- compile_instructions(model)
    system <- ChatMessage(role = "system",
                          content = textify(instructions, TextFormat()))
    input$system <- NULL
    Chat(model, messages = c(system, input), env = env)
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
           paste(unlist(lapply(x@tools, \(binding) {
               if (!is.null(binding@instructions) ||
                       length(binding@tool@examples) > 0L)
                   paste0("Specific instructions for '", binding@tool@name,
                          "':\n",
                          paste(c(binding@instructions,
                                  describe_examples(binding@tool@examples,
                                                    binding@io@input)),
                                collapse = "\n"))
           })), collapse = "\n\n"))
}

method(instructions,
       list(TextFormat, LanguageModel)) <- function(on, to)
{
    NULL
}

describe_examples <- function(ex, format = TextFormat()) {
    if (length(ex) > 0L) {
        ex <- vapply(ex, textify, character(1L), format)
        paste0("Example(s):\n\n",
               paste0(names(ex), "\nwould be encoded as:\n", ex,
                      collapse = "\n\n"))
    }
}

append_examples <- function(prompt, on) {
    paste(c(prompt, describe_examples(on@examples, on)), collapse = "\n")
}

method(instructions, list(JSONFormat, LanguageModel)) <- function(on, to) {
    prompt <- paste0("Return only JSON, without any explanation or other text. ",
                     "If the user input is incompatible with the task, ",
                     "issue an informative refusal.\n")
    ## object types are handled formally by backends, so we skip those
    if (length(on@schema) > 0L && !identical(on@schema$type, "object"))
        prompt <- paste0(prompt,
                         "The JSON must conform to the following schema:\n\n",
                         toJSON(on@schema),
                         "\n\n")
    append_examples(prompt, on)
}

method(instructions, list(CSVFormat, LanguageModel)) <- function(on, to) {
    prompt <- paste("Return only CSV, with values separated by commas.",
                    "Do not embed in markdown and do not send any other text.\n")
    if (length(names(on@col_classes)) > 0L)
        prompt <- paste0(prompt,
                         "The CSV should contain these columns: ",
                         paste0("\"", names(on@col_classes), "\"",
                                ifelse(is.na(on@col_classes), "",
                                       paste0(" (", on@col_classes, ")")),
                                collapse = ", "))
    append_examples(prompt, on)
}

method(instructions, list(CodeFormat, LanguageModel)) <- function(on, to) {
    prompt <- paste(c("Return only", on@language,
                      "code in markdown-style blocks,",
                      "without any explanation or other text.\n"),
                    collapse = " ")
    append_examples(prompt, on)
}

tool_input_format <- new_generic("tool_input_format", "x",
                                 function(x, tool, ...) S7_dispatch())

method(tool_input_format, LanguageModel) <- function(x, tool) {
    tool_input_json_format(tool)
}

tool_output_format <- new_generic("tool_output_format", "x",
                                  function(x, tool, ...) S7_dispatch())

method(tool_output_format, LanguageModel) <- function(x, tool) {
    tool_output_json_format(tool)
}

bind <- new_generic("bind", c("x", "to"))

method(bind, list(Tool, LanguageModel)) <- function(x, to, instructions = NULL) {
    assert_string(instructions, null.ok = TRUE)
    io_binding <- TextProtocol(input = tool_input_format(to, x),
                               output = tool_output_format(to, x))
    ToolBinding(tool = x, io = io_binding, instructions = instructions)
}

## Idea for unambiguously communicating references to R
## variables. Small models struggle to interpret this correctly.
interpret_symbols <-function(x) {
    x@instructions <- "You are an assistant embedded in an R session, where users will reference variables using backticks (``). When responding to user requests and calling tools, always preserve these backticks around variable names to correctly identify them as R variables. Do not remove the backticks when passing variable names to tools or functions, as the backticks identify them as R variables. Do not wrap other types of strings in backticks. If backticks are present around a word, treat it as a variable name, and use it as-is when calling tools."
    x
}
