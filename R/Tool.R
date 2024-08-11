validate_arg_schema <- function(prop) {
    c(if (!identical(prop$type, "object"))
          "$type must be 'object'",
      validate_list(prop$properties, names = "named"))
}

Tool <- new_class("Tool", class_function,
                  properties = list(
                      name = new_string_property(),
                      description = new_string_property(nullable = TRUE),
                      arg_schema = new_property(class_list,
                                                validator = validate_arg_schema)
                  ),
                  validator = function(object) {
                      extra_args <- setdiff(names(object@arg_schema$properties),
                                            names(formals(object)))
                      if (length(extra_args))
                          paste("@arg_schema$properties contains extra args:",
                                paste(extra_args, collapse = ", "))
                  })

as_arg_schema <- new_generic("as_arg_schema", "x")

method(as_arg_schema, NULL) <- function(x, FUN, Rd) {
    if (!is.null(Rd))
        props <- lapply(Rd_args(Rd)[names(formals(FUN))],
                        function(x) list(description = x))
    else {
        props <- as.list(formals(FUN))
        props[] <- TRUE
    }
    props <- mapply(function(prop, default) {
        default <- deparse(default)
        if (nzchar(default))
            list(description =
                     paste(c(prop$description, "default:", default),
                           collapse = " "))
        else prop
    }, props, formals(FUN), SIMPLIFY = FALSE)
    list(type = "object", properties = props)
}

method(as_arg_schema, class_list) <- function(x, FUN, Rd) {
    schema <- as_arg_schema(NULL, FUN, Rd)
    extra_args <- setdiff(names(x), names(formals(FUN)))
    if (length(extra_args) > 0L)
        stop("Extra arguments in 'arg_schema': ",
             paste(extra_args, collapse = ", "))
    schema$properties[names(x)] <- mapply(function(xi, prop) {
        xi_schema <- as_json_schema(xi)
        if (is.list(prop))
            xi_schema$description <- paste(c(xi_schema$description,
                                             prop$description), collapse = " ")
        xi_schema
    }, x, schema$properties[names(x)], SIMPLIFY = FALSE)
    schema
}

Tool_from_function <- function(FUN, name = deparse(substitute(FUN)),
                               description = NULL, arg_schema = NULL)
{
    Rd <- Rd_for_function(FUN, name)
    if (is.null(description) && !is.null(Rd))
        description <- paste(Rd_description(Rd), "Return value:", Rd_value(Rd))
    arg_schema <- as_arg_schema(arg_schema, FUN, Rd)
    Tool(FUN, name = name, description = description, arg_schema = arg_schema)
}
