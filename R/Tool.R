validate_Tool <- function(self) {
    ## TODO: validate examples against Signature
    extra_sig_args <- setdiff(names(self@signature@parameters),
                              names(formals(self)))
    extra_example_args <- setdiff(names(unlist(self@examples,
                                               recursive = FALSE)),
                                  names(formals(self)))
    ex_problems <- unlist(lapply(self@examples, function(ex) {
        mapply(function(arg, cls, nm) {
            if (!inherits(arg, cls))
                paste0("example argument '", nm,
                       "' does not inherit from class '",
                       S7:::S7_class_name(cls), "'")
        }, ex, self@signature@parameters[names(ex)], names(ex)) 
    }))
    
    c(if (length(extra_sig_args))
        paste("@signature@parameters contains extra args:",
              paste(extra_sig_args, collapse = ", ")),
      
      if (length(extra_example_args))
          paste("@examples contains extra args:",
                paste(extra_example_args, collapse = ", ")),
      ex_problems
      )
}

Tool <- new_class("Tool", class_function,
                  properties = list(
                      name = prop_string,
                      description = nullable(prop_string),
                      signature = ToolSignature,
                      examples = new_list_property(of = class_list)
                  ),
                  validator = validate_Tool)

method(print, Tool) <- function(x, ...) {
    cat(S7:::obj_desc(x))
    cat("", x@name)
    print(x@signature)
    cat(cli::ansi_strtrim(paste("@description:", x@description)))
    cat("\n")
}

any_signature <- function(args) {
    args <- as.list(args)
    args[] <- list(class_any)
    if (!is.null(args$"..."))
        args$"..." <- class_list
    ToolSignature(parameters = args, value = class_any)
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
    force(name)
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
    if (!inherits(tool, Tool) && is.function(tool))
        tool <- eval(as.call(c(wizrd::tool, substitute(tool))))
    x@tools[[tool@name]] <- bind(tool, x, instructions)
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

tool_input_json_format <- function(tool) {
    Rd <- Rd_for_function(S7_data(tool), tool@name)
    args <- tool@signature@parameters
    formals <- formals(tool)[names(args)]
    
    if (!is.null(Rd))
        props <- lapply(Rd_args(Rd)[names(args)],
                        function(x) list(description = x))
    else {
        props <- as.list(formals)
        props[] <- TRUE
    }
    
    props <- Map(function(prop, default) {
        default <- deparse(default)
        if (nzchar(default))
            list(description =
                     paste(c(if (is.list(prop)) prop$description, "default:",
                             default),
                           collapse = " "))
        else prop
    }, props, formals)
    
    schema <- as_json_schema(args)
    
    schema$properties <- Map(function(arg_schema, prop) {
        if (is.list(prop))
            arg_schema$description <-
                paste(c(arg_schema$description, prop$description),
                      collapse = " ")
        if (length(arg_schema) == 0L)
            # workaround bug in Ollama that requires a 'description'
            arg_schema <- list(description = "")
        arg_schema
    }, schema$properties, props)

    JSONFormat(schema = schema)
}

tool_output_json_format <- function(tool) {
    Rd <- Rd_for_function(S7_data(tool), tool@name)
    Rd_description <- if (!is.null(Rd)) Rd_value(Rd)

    schema <- as_json_schema(tool@signature@value)
    desc <- c(Rd_description, schema$description)
    if (!is.null(desc))
        schema$description <- paste(desc, collapse = " ")
    
    JSONFormat(schema = schema)
}
