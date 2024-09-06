TextFormat <- new_class("TextFormat",
                        properties = list(
                            examples = new_list_property(named = TRUE)
                        ))

PlainTextFormat <- new_class("PlainTextFormat", TextFormat)

JSONFormat <- new_class("JSONFormat", PlainTextFormat,
                        properties = list(schema = class_list))

CSVFormat <- new_class("CSVFormat", PlainTextFormat,
                       properties = list(schema = class_list,
                                         examples = new_list_property(
                                             of = class_data.frame,
                                             named = TRUE
                                         )))

CodeFormat <- new_class("CodeFormat", PlainTextFormat,
                        properties = list(language = nullable(prop_string)))

respond_with_format <- function(x, format = TextFormat()) {
    set_props(x, response_format = format)
}

respond_with_json <- function(x, schema = list(), examples = list()) {
    respond_with_format(x, JSONFormat(schema = schema, examples = examples))
}

respond_with_csv <- function(x, schema = list(), examples = list()) {
    respond_with_format(x, CSVFormat(schema = schema, examples = examples))
}

respond_with_code <- function(x) {
    respond_with_format(x, CodeFormat())
}

textify <- new_generic("textify", c("x", "format"))

method(textify, list(class_any, TextFormat)) <- function(x, format) {
    paste(capture.output(dput(x)), collapse = "\n")
}

method(textify, list(class_character, TextFormat)) <- function(x, format) {
    unname(x)
}

method(textify, list(class_list, TextFormat)) <- function(x, format) {
    lapply(unname(x), textify, format)
}

nativeRaster <- new_S3_class("nativeRaster")
raster <- new_S3_class("raster")
union_raster <- new_union(nativeRaster, raster)

method(textify, list(union_raster, TextFormat)) <- function(x, format) x

jsonify <- new_generic("jsonify", "x")

jsonify_s3 <- function(x) {
    list(.s3class = class(x), .data = x)
}

method(jsonify, class_any) <- jsonify_s3

native_json_classes <- NULL | class_logical | class_integer | class_double |
    class_character | class_data.frame

method(jsonify, native_json_classes) <- identity

method(jsonify, class_list) <- function(x) lapply(x, jsonify)

method(jsonify, S7_object) <- function(x) {
    prop_jsonify <- function(property) {
        val <- prop(x, property$name)
        if (inherits(property, scalar_S7_property))
            val <- unbox(val)
        else if (is.object(val))
            jsonify(val)
        else val
    }
    .data <- S7_data(x)
    c(.class = S7:::S7_class_name(S7_class(x)),
      if (typeof(.data) != "object") list(.data = .data),
      lapply(S7_class(x)@properties, prop_jsonify))
}

method(textify, list(class_list | class_any, JSONFormat)) <- function(x, format)
{
    toJSON(jsonify(x), null = "null")
}

## avoid evaluating arbitrary code from the model
constructor_from_name <- function(name) {
    tokens <- strsplit(name, "::", fixed = TRUE)[[1L]]
    if (length(tokens) == 1L)
        env <- .GlobalEnv
    env <- as.environment(paste0("package:", tokens[1L]))
    get(tokens[2L], env, mode = "function")
}

dejsonify <- new_generic("dejsonify", "x")

s7_dejsonify <- function(x) {
    constructor <- constructor_from_name(x$.class)
    x$.class <- NULL
    do.call(constructor, lapply(x, dejsonify))
}

parse_json_function <- function(fun) {
    as.function(parse(text=paste(fun, collapse = "\n"))[-1L], .GlobalEnv)
}

parse_json_expression <- function(expr) {
    as.expression(lapply(expr, \(x) parse(text = x)))
}

s3_dejsonify <- function(x) {
    s3class <- x$.s3class
    x$.s3class <- NULL
    data <- dejsonify(x$.data)
    conv <- switch(s3class, factor = as.factor, Date = as.Date,
                   POSIXct = as.POSIXct, complex = as.complex,
                   raw = {
                       require_ns("base64enc", "decode raw vectors from JSON")
                       base64enc::base64decode
                   }, `function` = parse_json_function,
                   expression = parse_json_expression, formula = as.formula)
    if (!is.null(conv))
        conv(data)
    else structure(data, class = s3class)
}

method(dejsonify, class_list) <- function(x) {
    if (!is.null(x$.class))
        s7_dejsonify(x)
    else if (!is.null(x$.s3class))
        s3_dejsonify(x)
    else lapply(x, dejsonify)
}

method(dejsonify, class_any) <- identity

detextify <- new_generic("detextify", c("x", "format"))

method(detextify, list(class_character, JSONFormat)) <- function(x, format) {
    detextify(fromJSON(x))
}

method(detextify, list(class_list, JSONFormat)) <- function(x, format) {
    dejsonify(x)
}

method(detextify, list(class_any, TextFormat)) <- function(x, format) x

Example <- new_class("Example",
                     properties = list(
                         property1 = new_property(class_numeric,
                                                  default = 42),
                         property2 = new_property(class_character,
                                                  default = "Hello, World!"),
                         property3 = new_property(class_logical,
                                                  default = TRUE)))

default_example <- function(class) {
    ex <- if (identical(class, S7_object))
        Example()
    else class()
    desc <- paste0("An object with this structure:\n", capture.output(str(ex)))
    setNames(list(ex), desc)
}

output_object <- function(x, class = S7_object, description = NULL,
                          example = NULL, ...)
{
    if (is.null(example))
        example <- default_example(class)
    stopifnot(is.list(examples),
              all(vapply(examples, inherits, logical(1L), class)))
    schema <- as_json_schema(class, description, ...)
    example <- as_json(example)
    format <- JSONFormat(schema = schema, example = example)
    x@io@output <- format
    x
}

method(textify, list(class_any, CodeFormat)) <- function(x, format) {
    paste(c(paste(c("```", format@language), collapse = ""),
            deparse(x), "```\n"), collapse = "\n")
}
