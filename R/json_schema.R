as_json_schema <- new_generic("as_json_schema", "from")

method(as_json_schema, class_logical | class_list) <- function(from, ...) {
    from
}

## meta schema of S7 objects; used for S7_object
s7_schema <- list(
    type = "object",
    properties = list(
        ".data" = list(
            type = c("string", "number", "boolean", "array", "object", "null"),
            description = "Base R object representing the S7 object. Omit if the object does not inherit from a base class."
        )
    ),
    patternProperties = list(
        "^.*$" = list(
            type = c("string", "number", "boolean", "array", "object", "null"),
            description = "The S7 object's properties."
        )
    ),
    description = "An R S7 object."
)

valid_by_default <- function(prop) {
    is.null(prop$validator) || is.null(prop$validator(S7:::prop_default(prop)))
}

base_ancestor_class <- function(class) {
    while(!identical(class, S7_object)) {
        class <- class@parent
        if (inherits(class, S7_base_class))
            return(class)
    }
}

prop_read_only <- function(prop) {
    !is.null(prop$getter) && is.null(prop$setter)
}

method(as_json_schema, S7_class) <- function(from, description = NULL, ...) {
    if (identical(from, S7_object))
        return(s7_schema)
    Rd <- get_Rd(from@name)
    arg_descriptions <- if (!is.null(Rd)) Rd_args(Rd)
    props <- Filter(Negate(prop_read_only), from@properties)
    prop_schema <- Map(as_json_schema, props, arg_descriptions[names(from)],
                       MoreArgs = list(...))
    base_class <- base_ancestor_class(from)
    if (!is.null(base_class)) {
        desc <- "Base R object representing instances of the class"
        prop_schema$.data <-
            as_json_schema(base_class, description = desc)
    }
    schema <- list(type = "object", title = from@name,
                   properties = prop_schema)
    description <- c(description, if (!is.null(Rd)) Rd_description(Rd))
    schema$description <- paste(description, collapse = " ")
    schema$required <- names(Filter(Negate(valid_by_default), props))
    schema
}

method(as_json_schema, S7_union) <- function(from, descriptions = NULL, ...) {
    list(anyOf = Map(as_json_schema,
                     from$classes,
                     as.list(descriptions)[seq_along(from$classes)]
                     ))
}

base_json_schema_type <- function(from, scalar) {
    if (scalar)
        switch(from$class, logical = "boolean",
               integer = "integer", double = "number",
               complex =, raw =, character = "string")
    else switch(from$class, logical =, integer =,
                double =, character =, list =, complex =, raw =,
                expression =, environment = "array",
                name =, call =, `function` = "string")
}

s3_json_schema_type <- function(from, scalar) {
    string_classes <- c("Date", "factor", "POSIXt", "formula")
    if (any(string_classes %in% from$class)) {
        if (scalar) "string" else "array"
    } else if ("data.frame" %in% from$class)
        "object"
}

base_json_schema <- function(from, description = NULL, scalar = FALSE,
                             named = FALSE, type_mapper = base_json_schema_type)
{
    type <- type_mapper(from, scalar)
    if (is.null(type))
        return(setNames(c(list(), description), character()))
    if (named)
        type <- "object"
    schema <- list(type = type)
    if (type == "array")
        schema$items <- base_json_schema(from, scalar = TRUE,
                                         type_mapper = type_mapper)
    else if (named)
        schema$patternProperties$"^.*$" <-
            base_json_schema(from, scalar = TRUE, type_mapper = type_mapper)
    
    schema$description <- description
    
    schema$format <- if ("Date" %in% from$class)
                         "date"
                     else if ("POSIXt" %in% from$class)
                         "date-time"

    schema
}

method(as_json_schema, S7_base_class) <- function(from, description = NULL,
                                                  scalar = FALSE, named = FALSE)
{
    base_json_schema(from, description, scalar, named)
}

method(as_json_schema, NULL) <- function(from, description = NULL) {
    c(list(type = NULL), description = description)
}

method(as_json_schema, S7_any) <- function(from, description = NULL) {
    c(list(), description = description)
}

method(as_json_schema, S7_S3_class) <- function(from, description = NULL) {
    if (is.null(description))
        description <- paste("S3 object of class",
                             paste(from$class, collapse = ", "))
    base_json_schema(from, description, type_mapper = s3_json_schema_type)
}

method(as_json_schema, S7_property) <- function(from, description = NULL, ...) {
    schema <- as_json_schema(from$class, ...)
    schema$description <- paste(c(schema$description,
                                  description,
                                  if (!is.null(from$validator)) deparse(body())
                                  ), collapse = " ")
    schema
}

method(as_json_schema, scalar_S7_property) <- function(from, description = NULL)
{
    as_json_schema(super(from, S7_property), description, scalar = TRUE)
}

method(as_json_schema, string_S7_property) <- function(from, description = NULL)
{
    schema <- as_json_schema(super(from, scalar_S7_property), description)
    schema$enum <- from$choices
    schema
}

method(as_json_schema, list_S7_property) <- function(from, description = NULL)
{
    schema <- as_json_schema(super(from, scalar_S7_property), description,
                             named = from$named)
    c(schema, items = if (!is.null(from$of)) list(as_json_schema(from$of)))
}

method(as_json_schema, numeric_S7_property) <- function(from, description = NULL)
{
    schema <- as_json_schema(super(from, scalar_S7_property), description)
    c(schema, minimum = from$min, maximum = from$max)
}
