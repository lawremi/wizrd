as_json_schema <- new_generic("as_json_schema", "from")

method(as_json_schema, class_logical) <- function(from, ...) {
    assert_flag(from)
    from
}

method(as_json_schema, class_list) <- function(from, ...) {
    assert_character(names(from))
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
    additionalProperties = list(
        type = c("string", "number", "boolean", "array", "object", "null"),
        description = "The S7 object's properties."
    ),
    description = "An R S7 object."
)

base_ancestor_class <- function(class) {
    while(!is.null(class)) {
        class <- class@parent
        if (inherits(class, S7_base_class))
            return(class)
    }
}

method(as_json_schema, S7_class) <- function(from, description = NULL, ...) {
    if (identical(from, S7_object))
        return(s7_schema)
    Rd <- get_Rd(from@name)
    arg_descriptions <- if (!is.null(Rd)) Rd_args(Rd) else list()
    props <- Filter(Negate(prop_read_only), from@properties)
    prop_schema <- Map(as_json_schema, props, arg_descriptions[names(props)]) |>
        ensure_named()
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
    if (!is.null(description))
        schema$description <- paste(description, collapse = " ")
    schema$required <- I(names(prop_schema))
    schema
}

method(as_json_schema, S7_union) <- function(from, descriptions = NULL, ...) {
    schemas <- Map(as_json_schema, from$classes,
                   as.list(descriptions)[seq_along(from$classes)])
    list(anyOf = schemas,
         title = paste(unlist(lapply(schemas, `[[`, "title")), collapse=" or "))
}

json_schema_type := new_generic("from")

method(json_schema_type, S7_base_class) <- function(from) {
    switch(from$class, logical = "boolean",
           integer = "integer", double = "number",
           complex =, raw =, character =,
           name =, call =, `function` =  "string")
}

method(json_schema_type, S7_S3_class) <- function(from) {
    string_classes <- c("Date", "factor", "POSIXt", "formula")
    if (any(string_classes %in% from$class)) {
        "string"
    } else if ("data.frame" %in% from$class)
        "object"
    else if ("matrix" %in% from$class) # arrays not handled because could be 1D
        "array"
}

default_description := new_generic("from")

method(default_description, S7_base_class) <- function(from) { 
    switch(from$class, 
           complex = "complex number",
           name = "name of an R variable", call = "call to an R function",
           `function` = "code defining an R function")
}

method(default_description, S7_S3_class) <- function(from) {
    if ("formula" %in% from$class)
        "R formula"
}

base_json_schema <- function(from, description = NULL, scalar = FALSE,
                             named = FALSE)
{
    schema <- named_list()
    type <- if (named && "list" %in% from$class)
                "object"
    else if (!scalar) "array"
    else json_schema_type(from)    
    schema$description <- description %||% if (scalar) default_description(from)
    if (is.null(type))
        return(schema)
    schema$type <- type
    if (type == "array" && !scalar)
        schema$items <- base_json_schema(from, scalar = TRUE)
    else if (type == "object")
        schema$additionalProperties <- TRUE
    
    if (scalar) {
        schema$format <- if ("Date" %in% from$class)
                             "date"
        else if ("POSIXt" %in% from$class)
            "date-time"
        else if ("raw" %in% from$class)
            "binary"
    }
    
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
    base_json_schema(from, description, scalar)
}

method(as_json_schema, S7_property) <- function(from, description = NULL, ...) {
    schema <- as_json_schema(from$class, ...)
    description <- c(schema$description, description)
    if (!is.null(description))
        schema$description <- paste(description, collapse = " ")
    schema
}

method(as_json_schema, scalar_S7_property) <- function(from, description = NULL)
{
    as_json_schema(s3_super(from, S7_property), description, scalar = TRUE)
}

method(as_json_schema, string_S7_property) <- function(from, description = NULL)
{
    schema <- as_json_schema(s3_super(from, scalar_S7_property), description)
    schema$enum <- from$choices
    schema
}

method(as_json_schema, list_S7_property) <- function(from, description = NULL)
{
    schema <- as_json_schema(s3_super(from, S7_property), description,
                             named = isTRUE(from$named))
    if (!is.null(from$of)) {
        of_schema <- as_json_schema(from$of)
        if (schema$type == "object")
            schema$additionalProperties <- of_schema
        else if (schema$type == "array")
            schema$items <- of_schema
    }
    schema
}

method(as_json_schema, data_frame_S7_property) <- function(from,
                                                           description = NULL)
{
    as_json_schema(from$prototype, description)
}

method(as_json_schema, numeric_S7_property) <- function(from,
                                                        description = NULL)
{
    schema <- as_json_schema(s3_super(from, scalar_S7_property), description)
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
    if (is.logical(x) || !identical(x$type, "object"))
        list(type = "object", properties = list("__boxed" = x),
             additionalProperties = FALSE,
             required = I("__boxed"))
    else x
}

norm_json_schema <- function(x) {
    if (is.list(x) && !is.null(x$required)) {
        x$required <- I(x$required)
        lapply(x, norm_json_schema)
    }
    x
}

schema_class <- function(x) {
    if (is.null(x) || isTRUE(x))
        return(class_any)
    if (!is.null(x$anyOf))
        return(do.call(new_union, lapply(x$anyOf, schema_class)))
    if (is.null(x$type))
        return(class_any)
    if (x$type == "object" && identical(names(x$properties), "__boxed"))
        x <- x$properties$"__boxed"
    switch(x$type,
           object = schema_S7_class(x),
           array = schema_array(x$items),
           string = scalar(class_character),
           number = scalar(class_numeric),
           integer = scalar(class_integer),
           boolean = scalar(class_logical))
}

schema_S7_class <- function(x, ...) {
    new_class(x$"$id" %||% x$title %||% "schema",
              properties = c(lapply(x$properties, schema_class),
                             `_dots` = if (isTRUE(x$additionalProperties))
                                 class_list),
              ...)
}

vectorize_property <- function(x) {
    if (inherits(x, scalar_S7_property))
        new_property(x$class)
    else list_of(x)
}

schema_array <- function(x) {
    item_class <- schema_class(x)
    if (inherits(item_class, scalar_S7_property))
        item_class$class
    else if (inherits(item_class, S7_class)) {
        vector_props <- lapply(item_class@properties, vectorize_property)
        vector_class <- new_class(item_class@name, properties = vector_props)
        prototype <- as.data.frame(as.list(formals(vector_class)))
        new_data_frame_property(prototype = prototype)
    }
    else list_of(item_class)
}
