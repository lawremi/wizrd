LanguageModelTool := new_class(
    Tool,
    properties = list(
        name = new_string_property(default = quote(model@name)),
        description = nullable(new_string_property(
            default = quote(model@instructions)
        )),
        model = new_property(
            LanguageModel,
            setter = \(self, value) {
                self@model <- value
                self@signature <- model_signature(value)
                self
            }
        ),
        signature = new_property(
            ToolSignature,
            setter = \(self, value) {
                S7_data(self, check = FALSE) <- model_fun(value)
                self@signature <- value
                self@examples <- self@model@examples
                self
            }
        )
    )
)

model_fun <- function(sig) {
    args <- formals(sig@parameters)
    if (length(args) != 1L)
        stop("'signature' must have a single parameter")
    predict_call <- as.call(c(quote(predict), quote(self@model),
                              as.name(names(args))))
    body <- substitute({
        self <- sys.function()
        if (!inherits(self, LanguageModelTool))
            stop("model function must be a LanguageModelTool")
        PREDICT_CALL
    }, list(PREDICT_CALL = predict_call))
    as.function(c(args, body))
}

model_signature <- function(model) {
    parameters <- list(x = convert(model@io@input, S7_property))
    value <- convert(model@io@output, S7_class)
    ToolSignature(parameters = parameters, value = value)
}

method(convert, list(LanguageModel, Tool)) <-
    function(from, to, name = from@name, description = from@instructions, ...) {
        LanguageModelTool(name = name, model = from, description = description,
                          ...)
    }
