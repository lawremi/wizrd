AgentTool := new_class(
    Tool,
    properties = list(
        name = scalar(class_character, default = quote(model@name)),
        description = nullable(scalar(
            class_character,
            default = quote(model@instructions)
        )),
        model = new_property(
            Agent,
            setter = \(self, value) {
                self@model <- value
                self@signature <- model_signature(value)
                self
            }
        ),
        signature = new_property(
            ToolSignature,
            setter = \(self, value) {
                S7_data(self, check = FALSE) <- model_fun(self@model, value,
                                                          self@predict_args)
                self@signature <- value
                self@examples <- self@model@examples
                self
            }
        ),
        predict_args = new_property(
            class_list,
            default = quote(list()),
            setter = \(self, value) {
                S7_data(self, check = FALSE) <- model_fun(self@model,
                                                          self@signature,
                                                          value)
                self@predict_args <- value
                self
            }
        )
    )
)

model_fun <- function(model, sig, predict_args) {
    args <- formals(sig@parameters)
    if (unary(model)) {
        stopifnot(length(args) == 1L)
        args_language <- as.name(names(args))
    } else {
        args_language <- as.call(c(quote(list), sapply(names(args), as.name)))
    }
    predict_call <- as.call(c(quote(predict), quote(self@model), args_language,
                              predict_args))
    body <- substitute({
        self <- sys.function()
        if (!inherits(self, AgentTool))
            stop("model function must be an AgentTool")
        PREDICT_CALL
    }, list(PREDICT_CALL = predict_call))
    as.function(c(args, body))
}

unary := new_generic("x")

method(unary, Agent) <- function(x) unary(x@io@input)

method(unary, GlueFormat) <- function(x) FALSE

method(unary, TextFormat) <- function(x) TRUE

as_parameters <- function(x) {
    if (unary(x))
        list(x = convert(x, S7_property))
    else convert(x, S7_class)
}

model_signature <- function(model) {
    parameters <- as_parameters(model@io@input)
    value <- convert(model@io@output, S7_property)
    ToolSignature(parameters = parameters, value = value)
}

method(convert, list(Agent, Tool)) <-
    function(from, to, name = from@name, description = from@instructions, ...) {
        AgentTool(name = name, model = from, description = description, ...)
    }

method(convert, list(Agent, class_function)) <- function(from, to, ...) {
    convert(from, Tool, ...)
}

method(as.function, Agent) <- function(x, ...) convert(x, class_function, ...)
