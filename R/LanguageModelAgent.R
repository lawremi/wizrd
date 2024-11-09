LanguageModelAgent <- new_class("LanguageModelAgent", Tool,
                                properties = list(
                                    model = LanguageModel
                                ))

agent_fun <- function(args) {
    body <- quote({
        self <- sys.function()
        if (!inherits(self, LanguageModelAgent))
            stop("agent function must be a LanguageModelAgent")
        predict(bind(self@model, self), mget(names(args)))
    })
    as.function(c(list(body), args))
}

## TODO: Move to a builder API that uses the same verbs as models:
##       accept_as(), output_as(), instruct(), etc. Actually, maybe
##       that applies to ALL tools. Then, by default, we are just
##       wrapping the Model as current configured, with the builder
##       API overriding that behavior.

agent <- function(model, args = alist(x =), signature = any_signature(args),
                  name = model@name, description = NULL, examples = list())
{
    LanguageModelAgent(agent_fun(args), name = name,
                       description = description,
                       signature = signature, examples = examples, model = model)
}

method(bind, list(LanguageModel, LanguageModelAgent)) <- function(x, to)
{
    x@instructions <- instructions(to, x)
    x@io <- agent_io_binding(x, to)
    x
}

method(instructions, list(LanguageModelAgent, LanguageModel)) <- function(on, to)
{
    paste0(c(
        "You are an agent named '", on@name, "'. ",
        "You are represented by an R function.",
        if (!is.null(on@description))
            paste0("\n\nYour task is:\n", on@description, "\n\n"), 
        "The user will pass a list of arguments to you.",
        "You will perform your task on the input and return the result.\n\n"
    ), collapse = "")
}

agent_input_format <- new_generic("agent_input_format", "x",
                                  function(x, tool, ...) S7_dispatch())

method(agent_input_format, LanguageModel) <- function(x, tool) {
    tool_input_format(tool)
}

agent_output_format <- new_generic("agent_output_format", "x",
                                   function(x, tool, ...) S7_dispatch())

method(agent_output_format, LanguageModel) <- function(x, tool) {
    tool_output_format(tool)
}

agent_io_binding <- function(x, tool) {
    TextProtocol(input = agent_input_format(x, tool),
                 output = agent_output_format(x, tool))
}

become <- function(model, tool) {
    do.call(LanguageModelAgent,
            c(agent_fun(function_formals(tool)), props(tool), model = model))
}
