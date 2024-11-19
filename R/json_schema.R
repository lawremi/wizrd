as_json_schema <- new_generic("as_json_schema", "from")

method(as_json_schema, class_logical | class_list) <- function(from, ...) {
    from
}

## meta schema of S7 objects; used for S7_object
s7_schema <- list(
    title = "S7_object",
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
    arg_descriptions <- if (!is.null(Rd)) Rd_args(Rd) else list()
    props <- Filter(Negate(prop_read_only), from@properties)
    prop_schema <- Map(as_json_schema, props, arg_descriptions[names(props)])
    base_class <- base_ancestor_class(from)
    if (!is.null(base_class)) {
        desc <- "Base R object representing instances of the class"
        prop_schema$.data <-
            as_json_schema(base_class, description = desc)
    }
    schema <- list(type = "object", title = from@name,
                   properties = prop_schema,
                   additionalProperties = FALSE)
    description <- c(description, if (!is.null(Rd)) Rd_description(Rd))
    schema$description <- paste(description, collapse = " ")
    schema$required <- I(names(props))
    schema
}

method(as_json_schema, S7_union) <- function(from, descriptions = NULL, ...) {
    schemas <- Map(as_json_schema, from$classes,
                   as.list(descriptions)[seq_along(from$classes)])
    list(anyOf = schemas,
         title = paste(unlist(lapply(schemas, `[[`, "title")), collapse=" or "))
}

base_json_schema_type <- function(from) {
    switch(from$class, logical = "boolean",
           integer = "integer", double = "number",
           complex =, raw =, character =,
           name =, call =, `function` =  "string")
}

s3_json_schema_type <- function(from) {
    string_classes <- c("Date", "factor", "POSIXt", "formula")
    if (any(string_classes %in% from$class)) {
        "string"
    } else if ("data.frame" %in% from$class)
        "object"
    else if ("matrix" %in% from$class) # arrays not handled because could be 1D
        "array"
}

base_json_schema <- function(from, description = NULL, scalar = FALSE,
                             named = FALSE, type_mapper = base_json_schema_type)
{
    schema <- list()
    type <- if (named && "list" %in% from$class)
                "object"
            else if (!scalar) "array"
            else type_mapper(from)
    if (is.null(type))
        return(c(schema, description = description))
    schema$type <- type
    if (type == "array" && !scalar)
        schema$items <- base_json_schema(from, scalar = TRUE,
                                         type_mapper = type_mapper)
    else if (type == "object")
        schema$patternProperties$"^.*$" <- list()
    
    schema$description <- description
    
    schema$format <- if ("Date" %in% from$class)
                         "date"
                     else if ("POSIXt" %in% from$class)
                         "date-time"
                     else if ("raw" %in% from$class)
                         "binary"

    schema
}

method(as_json_schema, S7_base_class) <- function(from, description = NULL,
                                                  scalar = NULL, named = FALSE)
{
    scalar <- scalar %||% (from$class %in% c("name", "call", "function"))
    base_json_schema(from, description, scalar, named)
}

method(as_json_schema, NULL) <- function(from, description = NULL) {
    c(list(type = NULL), description = description)
}

method(as_json_schema, S7_any) <- function(from, description = NULL) {
    c(list(), description = description)
}

method(as_json_schema, S7_S3_class) <- function(from, description = NULL,
                                                scalar = NULL)
{
    known_vector_classes <- c("Date", "POSIXt", "factor", "data.frame",
                              "matrix", "array")
    if (is.null(scalar))
        scalar <- !any(from$class %in% known_vector_classes)
    base_json_schema(from, description, scalar,
                     type_mapper = s3_json_schema_type)
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

method(as_json_schema, data_frame_S7_property) <- function(from,
                                                           description = NULL)
{
    as_json_schema(from$prototype, description)
}

method(as_json_schema, numeric_S7_property) <- function(from,
                                                        description = NULL)
{
    schema <- as_json_schema(super(from, scalar_S7_property), description)
    c(schema, minimum = from$min, maximum = from$max)
}

json_schema_for_object <- function(x, ...) as_json_schema(class_object(x), ...)

method(as_json_schema, class_data.frame) <- function(from, description = NULL)
{
    schema <- list(title = "data_frame",
                   type = "array",
                   items = list(
                       type = "object",
                       properties = lapply(from, json_schema_for_object,
                                           scalar = TRUE),
                       additionalProperties = FALSE,
                       required = I(names(from)))
                   )
    schema$description <- description
    schema
}

box_json_schema <- function(x) {
    if (!identical(x$type, "object"))
        list(type = "object", properties = list("__boxed" = x),
             additionalProperties = FALSE,
             required = I("__boxed"))
    else x
}
