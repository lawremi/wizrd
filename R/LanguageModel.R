LanguageModel <- new_class("LanguageModel",
                           properties = list(
                               backend = LanguageModelBackend,
                               name = nullable(prop_string),
                               instructions = new_string_property(
                                   default = "You are a helpful assistant."
                               ),
                               io = TextProtocol,
                               tools = new_list_property(of = ToolBinding),
                               params = LanguageModelParams,
                               examples = new_data_frame_property(
                                   col.names = c("input", "output")
                               ),
                               system_prompt_format = new_property(
                                   TextFormat,
                                   default = paste(c("{tool_instructions}",
                                                     "{output_instructions}",
                                                     "{task_instructions}"),
                                                   collapse = "\n\n") |>
                                       GlueFormat() |> quote()
                               )
                           ))

chat <- new_generic("chat", "x")
embed_text <- new_generic("embed_text", "x")

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
    cat("@backend: ")
    print(x@backend)
}

method(perform_chat, LanguageModel) <- function(x, messages, stream_callback,
                                                env, ...)
{
    perform_chat(x@backend, x@name, messages, x@tools, x@io,
                 set_props(x@params, ...), stream_callback)
}

method(chat, LanguageModel) <- function(x, input = NULL, stream_callback = NULL,
                                        system_params = list(),
                                        env = parent.frame(), ...)
{
    chat(convert(x, Chat, env = env, system_params = system_params), input,
         stream_callback, ...)
}

predict_via_chat <- function(object, input, env = parent.frame(), ...)
{
    last_output(chat(object, input, env = env, ...))
}

method(predict, LanguageModel) <- predict_via_chat

instruct <- function(x, ...) {
    set_props(x, instructions = paste0(...))
}

textify_system_prompt <- function(x, params = list()) {
    all_params <- c(
        tool_instructions = tool_instructions(x),
        output_instructions = instructions(x@io@output, x),
        task_instructions = x@instructions
    )
    all_params[names(params)] <- params
    textify(all_params, x@system_prompt_format) |> trimws()
}

instructions <- new_generic("instructions", c("on", "to"))

tool_instructions <- new_generic("tool_instructions", "x")

method(tool_instructions, LanguageModel) <- function(x) {
    if (length(x@tools) == 0L)
        return("")
    
    paste0("You have access to one or more tools named ",
           paste(names(x@tools), collapse = ", "),
           ". Use them when relevant to answer user queries, but ",
           "prioritize generating responses without them unless ",
           "the user explicitly requests or ",
           "the task requires precise computation or external data.\n\n",
           paste(unlist(lapply(x@tools, \(binding) {
               if (!is.null(binding@instructions))
                   paste0("Specific instructions for '", binding@tool@name,
                          "':\n",
                          paste(binding@instructions, collapse = "\n"))
           })), collapse = "\n\n"))
}

method(instructions, list(TextFormat, LanguageModel)) <- function(on, to) ""

describe_examples <- function(ex, io = TextProtocol()) {
    if (nrow(ex) == 0L)
        return(NULL)
    ex_text <- 
        paste0("Input: ", vapply(exi$input, textify, character(1L), io@input),
               "\n",
               "Output: ", vapply(exi$output, textify, character(1L), io@output),
               collapse = "\n\n")
    paste0("Example(s):\n\n", ex_text)
}

method(instructions, list(JSONFormat, LanguageModel)) <- function(on, to) {
    prompt <- paste0("Return only JSON, without any explanation or other text. ",
                     "If the user input is incompatible with the task, ",
                     "issue an informative refusal.\n")
    prompt
}

json_type <- function(x) {
    vswitch(x, logical = "boolean",
            integer = "integer", numeric = "number", double = "number",
            complex = "string", character = "string", factor = "string",
            Date = "date in ISO8601 format",
            POSIXct = "date-time in ISO8601 format")
}

method(instructions, list(CSVFormat, LanguageModel)) <- function(on, to) {
    prompt <- paste("Return only CSV, with values separated by commas.",
                    "Include a header containing the column names.",
                    "The CSV should adhere to the RFC 4180 standard,",
                    "so use double quotes (\"\") to escape quotes.",
                    "Never use \\ to escape quotes.",
                    "Do not embed in markdown and do not send any other text.\n")
    if (length(names(on@col_classes)) > 0L)
        prompt <- paste0(prompt,
                         "The CSV should contain these columns: ",
                         paste0("\"", names(on@col_classes), "\"",
                                ifelse(is.na(on@col_classes), "",
                                       paste0(" (", json_type(on@col_classes),
                                              ")")),
                                collapse = ", "))
    prompt
}

method(instructions, list(CodeFormat, LanguageModel)) <- function(on, to) {
    prompt <- paste(c("Return only", on@language,
                      "code. Do not wrap the code in ``` blocks.",
                      "Do not include any explanation or other text.\n"),
                    collapse = " ")
    prompt
}

tool_input_format <- new_generic("tool_input_format", "x",
                                 function(x, tool, ...) S7_dispatch())

method(tool_input_format, LanguageModel) <- function(x, tool) {
    tool_input_json_format(tool)
}

tool_output_format <- new_generic("tool_output_format", "x",
                                  function(x, tool, ...) S7_dispatch())

method(tool_output_format, LanguageModel) <- function(x, tool) {
    PlainTextFormat()
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

method(embed_text, LanguageModel) <- function(x, data, ndim = NULL) {
    assert_int(ndim, null.ok = TRUE)

    if (length(data) == 0L)
        return(matrix(numeric(), ncol = ndim))
    
    data <- textify(data, x@io@input)
    if (length(data) == 1L && (!is.list(data) || is.object(data)))
        data <- list(data)
    if (any(lengths(data) != 1L))
        stop("elements of 'data' must be length one")
    
    do.call(rbind, lapply(data, perform_embedding, model = x@name, x = x@backend,
                          ndim = ndim))
}

method(on_restore, LanguageModel) <- function(x, ...) {
    set_props(x, backend = on_restore(x@backend, x@name, ...))
}

demonstrate <- function(x, examples = data.frame(input, output),
                        input = character(), output = character())
{
    assert_data_frame(examples, col.names = c("input", "output"))
    x@examples <- rbind(x@examples, examples)
    x
}

method(textify, list(class_any, LanguageModel)) <- function(x, format) {
    textify(x, format@io@input)
}

method(detextify, list(class_any, LanguageModel)) <- function(x, format) {
    detextify(x, format@io@output)
}
