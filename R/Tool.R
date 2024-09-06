validate_Tool <- function(self) {
    ## TODO: validate examples against Signature
    extra_sig_args <- setdiff(names(self@signature@arguments),
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
        }, ex, self@signature@arguments[names(ex)], names(ex)) 
    }))
    
    c(if (length(extra_sig_args))
        paste("@signature@arguments contains extra args:",
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
    x@tools[[tool@name]] <- bind(tool, x, instructions)
    x
}

ToolBinding <- new_class("ToolBinding",
                         properties = list(
                             tool = Tool,
                             io = IOBinding,
                             instructions = nullable(prop_string)
                         ))

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
    
    props <- Map(function(prop, default) {
        default <- deparse(default)
        if (nzchar(default))
            list(description =
                     paste(c(if (is.list(prop)) prop$description, "default:",
                             default),
                           collapse = " "))
        else prop
    }, props, formals)
    
    schema <- list(type = "object", properties = props)

    schema$properties[names(args)] <- Map(function(arg, prop) {
        arg_schema <- as_json_schema(arg)
        if (is.list(prop))
            arg_schema$description <-
                paste(c(arg_schema$description, prop$description),
                      collapse = " ")
        arg_schema
    }, args, schema$properties[names(args)])

    JSONFormat(schema = schema)
}

tool_output_json_format <- function(tool) {
    Rd <- Rd_for_function(S7_data(tool), tool@name)
    Rd_description <- if (!is.null(Rd)) Rd_value(Rd)

    schema <- as_json_schema(tool@signature@value)
    schema$description <- paste(c(Rd_description, schema$description),
                                collapse = " ")
    
    JSONFormat(schema = schema)
}
