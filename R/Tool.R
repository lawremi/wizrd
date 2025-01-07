validate_Tool <- function(self) {
    example_problems <- unique(unlist(lapply(self@examples$input, \(ex) {
        if (!inherits(ex, self@signature@parameters))
            "examples must be instances of @signature@parameters"
    })))

    formal_names <- names(tool_formals(self))
    extra_arg_problems <- if (!"..." %in% formal_names) {
        extra_sig_args <- setdiff(names(self@signature@parameters@properties),
                                  formal_names)
        extra_param_descs <- setdiff(names(self@param_descriptions),
                                     formal_names)
        c(if (length(extra_sig_args))
            paste("@signature@parameters contains extra args:",
                  paste(extra_sig_args, collapse = ", ")),
          if (length(extra_param_descs))
              paste("@param_descriptions contains extra args:",
                    paste(extra_param_descs, collapse = ", ")))
    }
    c(example_problems, extra_arg_problems)
}

Tool <- new_class("Tool", class_function,
                  properties = list(
                      name = new_string_property(
                          default = quote(deparse(substitute(.data)))
                      ),
                      description = nullable(prop_string),
                      signature = new_property(
                          ToolSignature,
                          default = quote(any_signature(.data))
                      ),
                      param_descriptions = named(new_property(
                          class_character,
                          setter = \(self, value) {
                              self@param_descriptions <-
                                  norm_param_descriptions(value, self@signature)
                              self
                          }
                      )),
                      value_description = nullable(prop_string),
                      examples = new_data_frame_property(
                          col.names = c("input", "output"),
                          setter = \(self, value) {
                              self@examples <- norm_examples(value, self)
                              self
                          }
                      )
                  ),
                  validator = validate_Tool)

method(print, Tool) <- function(x, ...) {
    cat(S7:::obj_desc(x))
    cat("", x@name)
    print(x@signature)
    cat(cli::ansi_strtrim(paste("@description:", x@description)))
    cat("\n")
}

any_signature <- function(FUN) {
    args <- function_formals(FUN)
    params <- as.list(args)
    params[] <- list(class_any)
    if (!is.null(params$"..."))
        params$"..." <- class_list
    ToolSignature(value = class_any, parameters = params)
}

norm_examples <- function(examples, self) {
    FUN <- self
    if (is.primitive(FUN))
        FUN <- as_stub_closure(FUN)
    examples$input <- lapply(examples$input, \(input) {
        mc <- match.call(FUN, as.call(c(FUN, input)), expand.dots = FALSE) |>
            dodge_dots() |> as.list()
        do.call(self@signature@parameters, mc[-1L])
    })
    examples
}

norm_param_descriptions <- function(descs, signature) {
    if (length(descs) == 0L)
        names(descs) <- character()
    if (is.null(names(descs))) {
        sig_names <- names(signature@parameters@properties)
        if (length(descs) != length(sig_names))
            stop("if 'param_descriptions' is unnamed, it must be parallel to ",
                 "'signature'")
        names(descs) <- sig_names
    }
    descs |> dodge_dots()
}

add_Rd <- function(tool) {
    params_to_describe <- setdiff(names(tool@signature@parameters@properties),
                                  names(tool@param_descriptions))
    fully_described <- !is.null(tool@description) &&
        !is.null(tool@value_description) && length(params_to_describe) == 0L
    if (fully_described)
        return(tool)
    
    Rd <- Rd_for_function(S7_data(tool), tool@name)
    if (is.null(Rd))
        return(tool)

    if (is.null(tool@description))
        tool@description <- Rd_description(Rd)
    args <- Rd_args(Rd) |> dodge_dots()
    tool@param_descriptions[params_to_describe] <-
        as.character(args[params_to_describe])
    if (is.null(tool@value_description))
        tool@value_description <- Rd_value(Rd)

    tool
}

tool <- function(x, name = deparse(substitute(x)), ...) {
    convert(x, Tool, name = name, ...)
}

method(convert, list(class_function, Tool)) <- function(from, to, ...) {
    Tool(from, ...) |> add_Rd()
}

equip <- function(x, tool, instructions = NULL, ...) {
    stopifnot(inherits(x, LanguageModel))
    if (!inherits(tool, Tool))
        tool <- wizrd::tool(tool, name = deparse(substitute(tool)), ...)
    x@tools[[tool@name]] <- bind(tool, x, instructions)
    x
}

unequip <- function(x, name) {
    stopifnot(inherits(x, LanguageModel))
    x@tools[[name]] <- NULL
    x
}

can_accept_as <- function(`_x`, ..., `_parameters` = list(...)) {
    x <- `_x`
    parameters <- `_parameters`
    stopifnot(inherits(x, Tool))
    x@signature@parameters <- match.call(x, as.call(c(x, parameters)))[-1L] |>
        as.list()
    x
}

can_return_as <- function(x, type) {
    stopifnot(inherits(x, Tool))
    x@signature@value <- type
    x
}

ToolBinding <- new_class("ToolBinding",
                         properties = list(
                             tool = Tool,
                             io = TextProtocol,
                             instructions = nullable(prop_string)
                         ))

method(print, ToolBinding) <- function(x, ...) {
    cat(S7:::obj_desc(x))
    cat("\n@tool:", x@tool@name)
    cat("\n@io: "); print(x@io, ...)
    cat(cli::ansi_strtrim(paste("@instructions:", x@instructions)))
    cat("\n")
}

json_schema_add_defaults <- function(params, formals) {
    Map(function(param, default) {
        default <- deparse(default)
        if (nzchar(default))
            list(description =
                     paste(c(if (is.list(param)) param$description, "default:",
                             default),
                           collapse = " "))
        else param
    }, params, formals)
}

tool_formals <- function(tool) {
    function_formals(tool) |> dodge_dots()
}

json_schema_param_descs <- function(tool) {
    param_names <- names(tool@signature@parameters@properties)
    lapply(tool@param_descriptions[param_names], \(desc) {
        if (!is.na(desc))
            list(description = desc)
        else TRUE
    }) |> json_schema_add_defaults(tool_formals(tool)[param_names])
}

tool_input_json_schema <- function(sig_params, param_descs) {
    schema <- as_json_schema(sig_params)
    schema$properties <- Map(function(param_schema, param_desc) {
        if (is.list(param_desc))
            param_schema$description <-
                paste(c(param_schema$description, param_desc$description),
                      collapse = " ")
        if (length(param_schema) == 0L)
            # workaround bug in Ollama that requires a 'description'
            param_schema <- list(description = "")
        param_schema
    }, schema$properties, param_descs)
    schema$required <- NULL
    schema
}

tool_input_json_format <- function(tool) {
    sig_params <- tool@signature@parameters
    param_descs <- json_schema_param_descs(tool)[names(sig_params@properties)]

    schema <- tool_input_json_schema(sig_params, param_descs)
    
    JSONFormat(schema = schema, schema_class = sig_params)
}

## currently unused
tool_output_json_format <- function(tool) {
    schema <- as_json_schema(tool@signature@value)
    desc <- c(tool@value_description, schema$description)
    if (!is.null(desc))
        schema$description <- paste(desc, collapse = " ")
    
    JSONFormat(schema = schema,
               schema_class = tool@signature@value)
}

describe_tool <- function(binding) {
    paste(c(binding@tool@description, describe_tool_examples(binding)),
          collapse = "\n\n")
}

describe_tool_examples <- function(binding) {
    tool <- binding@tool
    ex <- tool@examples
    if (nrow(ex) == 0L)
        return(NULL)
    call <- lapply(ex$input, \(args) as.call(c(as.name(tool@name), props(args))))
    value <- vapply(ex$output, textify, character(1L), binding@io@output)
    ex_text <- paste0(call, " returns: ", value, collapse = "\n")
    paste0("Example(s):\n", ex_text)
}
