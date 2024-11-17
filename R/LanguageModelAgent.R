LanguageModelAgent := new_class(
    Tool,
    properties = list(
        model = new_property(
            LanguageModel,
            setter = \(self, value) {
                self@signature <- agent_signature(value)
                self@model <- value
                self
            }
        ),
        signature = new_property(
            ToolSignature,
            setter = \(self, value) {
                S7_data(self) <- agent_fun(value)
                self@signature <- value
                self
            }
        )
    )
)

agent_fun <- function(sig) {
    args <- formals(signature@parameters)
    if (length(args) != 1L)
        stop("'signature' must have a single parameter")
    predict_call <- as.call(c(quote(predict), quote(self@model),
                              as.name(names(args))))
    body <- substitute({
        self <- sys.function()
        if (!inherits(self, LanguageModelAgent))
            stop("agent function must be a LanguageModelAgent")
        PREDICT_CALL
    }, list(PREDICT_CALL = predict_call))
    as.function(c(body, args))
}

agent_signature <- function(model) {
    parameters <- list(x = convert(model@io@input, S7_property))
    value <- convert(model@io@output, S7_class)
    ToolSignature(parameters = parameters, value = value)
}

agent <- function(model, name = model@name)
{
    LanguageModelAgent(name = name, model = model,
                       description = model@instructions)
}
