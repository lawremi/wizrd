validate_Tool <- function(object) {
    ## TODO: validate examples against Signature
    extra_sig_args <- setdiff(names(object@signature@arguments),
                              names(formals(object)))
    extra_example_args <- setdiff(names(unlist(object@examples,
                                               recursive = FALSE)),
                                  names(formals(object)))
    ex_problems <- unlist(lapply(object@examples), function(ex) {
        mapply(function(arg, cls, nm) {
            if (!inherits(arg, cls))
                paste0("example argument '", nm,
                       "' does not inherit from class '",
                       S7:::S7_class_name(cls), "'")
        }, ex, object@signature@arguments[names(ex)], names(ex)) 
    })
    
    c(if (length(extra_sig_args))
        paste("@signature@arguments contains extra args:",
              paste(extra_sig_args, collapse = ", ")),
      
      if (length(extra_example_args))
          paste("@examples contains extra args:",
                paste(extra_example_args, collapse = ", "))
      ex_problems
      )
}

Tool <- new_class("Tool", class_function,
                  properties = list(
                      name = prop_string,
                      description = prop_string_nullable,
                      signature = ToolSignature,
                      examples = new_list_property(of = class_list,
                                                   named = TRUE)
                  ),
                  validator = validate_Tool)

any_signature <- function(args) {
    args <- as.list(args)
    args[] <- class_any
    if (!is.null(args$...))
        args$... <- class_list
    ToolSignature(arguments = args, value = class_any)
}

norm_examples <- function(examples, FUN) {
    lapply(examples, function(example) {
        as.list(match.call(FUN, as.call(c(list(FUN), example))))[-1L]
    })
}

tool <- function(FUN, signature = any_signature(formals(FUN)),
                 name = deparse(substitute(FUN)),
                 description = NULL, examples = list())
{
    FUN <- match.fun(FUN)
    if (is.null(description)) {
        Rd <- Rd_for_function(FUN, name)
        if (!is.null(Rd))
            description <- Rd_description(Rd)
    }
    examples <- norm_examples(examples, FUN)
    Tool(FUN, name = name, description = description, signature = signature,
         examples = examples)
}

equip <- function(x, tool, instructions = NULL) {
    x@tools[[tool@name]] <- bind_tool(tool, x, instructions)
    x
}

BoundTool <- new_class("BoundTool", Tool,
                       properties = list(
                           binding = FormatBinding,
                           instructions = prop_string_nullable
                       ))

bind_fun <- function(FUN) {
    function(args) {
        do.call(FUN, args)
    }
}

bind <- new_generic("bind", c("x", "to"))

method(bind, list(Tool, LanguageModel)) <- function(x, to, instructions = NULL) {
    assert_string(instructions, null.ok = TRUE)
    format_binding <- FormatBinding(input = tool_input_format(to, x),
                                    output = tool_output_format(to, x))
    do.call(BoundTool, c(bind_fun(x), props(x), binding = format_binding,
                         instructions = instructions))
}

method(bind, list(BoundTool, LanguageModel)) <- function(x, to) x

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

tool_input_json_format <- function(tool) {
    Rd <- Rd_for_function(S7_data(tool), tool@name)
    args <- tool@signature@arguments
    formals <- formals(tool)
    
    if (!is.null(Rd))
        props <- lapply(Rd_args(Rd)[names(formals)],
                        function(x) list(description = x))
    else {
        props <- as.list(formals)
        props[] <- TRUE
    }
    
    props <- mapply(function(prop, default) {
        default <- deparse(default)
        if (nzchar(default))
            list(description =
                     paste(c(if (is.list(prop)) prop$description, "default:",
                             default),
                           collapse = " "))
        else prop
    }, props, formals, SIMPLIFY = FALSE)
    
    schema <- list(type = "object", properties = props)

    schema$properties[names(args)] <- mapply(function(arg, prop) {
        arg_schema <- as_json_schema(arg)
        if (is.list(prop))
            arg_schema$description <-
                paste(c(arg_schema$description, prop$description),
                      collapse = " ")
        arg_schema
    }, args, schema$properties[names(args)], SIMPLIFY = FALSE)

    JSONFormat(format, schema = schema)
}

tool_output_json_format <- function(tool) {
    Rd <- Rd_for_function(S7_data(tool), tool@name)
    Rd_description <- if (!is.null(Rd)) Rd_value(Rd)

    schema <- as_json_schema(tool@signature@value)
    schema$description <- paste(c(Rd_description, schema$description),
                                collapse = " ")
    
    schema
}
