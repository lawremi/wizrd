SerialFormat <- new_class("SerialFormat",
                          properties = list(example = class_any))

JSONFormat <- new_class("JSONFormat", SerialFormat,
                        properties = list(schema = class_list,
                                          ## TODO: validate with jsonvalidate
                                          example = class_list))

CSVFormat <- new_class("CSVFormat", SerialFormat,
                       properties = list(schema = class_list,
                                         example = class_data.frame))

CodeFormat <- new_class("CodeFormat", SerialFormat,
                        properties = list(language = prop_string_nullable))

respond_with_format <- function(x, format = SerialFormat()) {
    set_props(x, response_format = format)
}

respond_with_json <- function(x, schema = list(), example = list()) {
    respond_with_format(x, JSONFormat(schema = schema, example = example))
}

input_instructions <- new_generic("input_instructions", c("on", "to"))

output_instructions <- new_generic("output_instructions", c("on", "to"))

method(input_instructions,
       list(SerialFormat, LanguageModel)) <- function(on, to)
{
    NULL
}

method(output_format_prompt,
       list(SerialFormat, LanguageModel)) <- function(on, to)
{
    NULL
}

method(output_instructions,
       list(JSONFormat, LanguageModel)) <- function(on, to)
{
    prompt <- "Return only JSON, without any explanation or other text.\n"
    if (length(on@schema) > 0L)
        prompt <- paste0(prompt,
                         "The JSON must conform to the following schema:\n\n",
                         toJSON(on),
                         "\nEnsure the JSON matches this schema exactly.\n")
    if (length(on@example) > 0L)
        prompt <- paste(prompt, "Here is an example:\n\n", toJSON(on@example))
    prompt
}

method(input_instructions,
       list(JSONFormat, LanguageModel)) <- function(on, to)
{
    prompt <- "The user will send only JSON, without any other text.\n"
    if (length(on@schema) > 0L)
        prompt <- paste0(prompt,
                         "The JSON will conform to the following schema:\n\n",
                         toJSON(on),
                         "\nExpect the JSON to match this schema exactly.\n")
    if (length(on@example) > 0L)
        prompt <- paste(prompt, "Here is an example:\n\n", toJSON(on@example))
    prompt
}

respond_with_csv <- function(x, schema = list(), example = data.frame()) {
    respond_with_format(x, CSVFormat(schema = schema, example = example))
}

method(output_instructions,
       list(CSVFormat, LanguageModel)) <- function(on, to)
{
    prompt <- "Return only CSV, without any explanation or other text.\n"
    if (length(on@schema) > 0L)
        prompt <- paste0(prompt,
                         "This JSON schema defines the structure of the data:\n",
                         toJSON(on),
                         "\nInterpret the JSON schema to understand ",
                         "the required columns and data types and produce ",
                         "the corresponding CSV. ",
                         "Ensure the CSV matches this schema exactly.\n")
    if (length(on@example) > 0L)
        prompt <- paste(prompt, "Here is an example:\n\n", toJSON(on@example))
    prompt
}

method(input_instructions,
       list(CSVFormat, LanguageModel)) <- function(on, to)
{
    prompt <- "The user will send only CSV, without any other text.\n"
    if (length(on@schema) > 0L)
        prompt <- paste0(prompt,
                         "This JSON schema defines the structure of the data:\n",
                         toJSON(on),
                         "\nInterpret the JSON schema to understand ",
                         "the expected columns and data types. ",
                         "Expect the CSV to match this schema exactly.\n")
    if (length(on@example) > 0L)
        prompt <- paste(prompt, "Here is an example:\n\n", toJSON(on@example))
    prompt
}

respond_with_code <- function(x) {
    respond_with_format(x, CodeFormat())
}

markdown_code_example <- function(format) {
    paste(c(paste(c("```", format@language), collapse = ""),
            deparse(format@example), "```\n"), collapse = "\n")
}

method(output_instructions,
       list(CodeFormat, LanguageModel)) <- function(on, to)
{
    prompt <- paste(c("Return only", on@language,
                      "code in markdown-style blocks,",
                      "without any explanation or other text.\n"),
                    collapse = " ")
    if (length(on@example) > 0L)
        prompt <- paste(prompt, "Here is an example:\n",
                        markdown_code_example(on))
    prompt
}

method(input_instructions,
       list(CodeFormat, LanguageModel)) <- function(on, to)
{
    prompt <- paste(c("The user will send only", on@language,
                      "code in markdown-style blocks,",
                      "without any explanation or other text.\n"),
                    collapse = " ")
    if (length(on@example) > 0L)
        prompt <- paste(prompt, "Here is an example:\n",
                        markdown_code_example(on))
    prompt
}

serialize <- new_generic("serialize", c("x", "format"))

method(serialize, c(class_any, SerialFormat)) <- function(x, format) {
    capture.output(dput(x))
}

method(serialize, c(class_list, SerialFormat)) <- function(x, format) {
    lapply(x, serialize, format)
}

nativeRaster <- new_S3_class("nativeRaster")
raster <- new_S3_class("raster")
union_raster <- new_union(nativeRaster, raster)

method(serialize, c(union_raster, SerialFormat)) <- function(x, format) x

to_json <- new_generic("to_json", "x")

to_json_s3 <- function(x) {
    list(.s3class = class(x), .data = x)
}

method(to_json, class_any) <- to_json_s3

method(to_json, class_vector) <- identity

method(to_json, class_complex | class_raw | class_expression) <- to_json_s3

method(to_json, S7_object) <- function(x) {
    prop_to_json <- function(property) {
        val <- prop(x, property$name)
        if (inherits(property, scalar_S7_property))
            val <- unbox(val)
        else if (is.object(val))
            to_json(val)
        else val
    }
    .data <- S7_data(x)
    c(.class = S7:::S7_class_name(S7_class(x)),
      if (typeof(.data) != "object") list(.data = .data),
      lapply(S7_class(x)@properties, prop_to_json))
}

method(serialize, list(class_list | class_any, JSONFormat)) <- function(x,
                                                                        format)
{
    toJSON(to_json(x), null = "null")
}

## avoid evaluating arbitrary code from the model
constructor_from_name <- function(name) {
    tokens <- strsplit(name, "::", fixed = TRUE)[[1L]]
    if (length(tokens) == 1L)
        env <- .GlobalEnv
    env <- as.environment(paste0("package:", tokens[1L]))
    get(tokens[2L], env, mode = "function")
}

from_json <- new_generic("from_json", "x")

s7_from_json <- function(x) {
    constructor <- constructor_from_name(x$.class)
    x$.class <- NULL
    do.call(constructor, lapply(x, from_json))
}

parse_json_function <- function(fun) {
    as.function(parse(text=paste(fun, collapse = "\n"))[-1L], .GlobalEnv)
}

parse_json_expression <- function(expr) {
    as.expression(lapply(expr, \(x) parse(text = x)))
}

s3_from_json <- function(x) {
    s3class <- x$.s3class
    x$.s3class <- NULL
    data <- from_json(x$.data)
    conv <- switch(s3class, factor = as.factor, Date = as.Date,
                   POSIXct = as.POSIXct, complex = as.complex,
                   raw = {
                       requireNamespace("base64enc")
                       base64enc::base64decode
                   }, `function` = parse_json_function,
                   expression = parse_json_expression)
    if (!is.null(conv))
        conv(data)
    else structure(data, class = s3class)
}

method(from_json, class_list) <- function(x) {
    if (!is.null(x$.class))
        s7_from_json(x)
    else if (!is.null(x$.s3class))
        s3_from_json(x)
    else lapply(x, from_json)
}

method(from_json, class_any) <- identity

method(deserialize, list(class_character, JSONFormat)) <- function(x, format) {
    deserialize(fromJSON(x))
}

method(deserialize, c(class_list, JSONFormat)) <- function(x, format) {
    from_json(x)
}

method(deserialize, c(class_any, SerialFormat)) <- identity

Example <- new_class("Example",
                     properties = list(
                         property1 = new_property(class_numeric,
                                                  default = 42),
                         property2 = new_property(class_character,
                                                  default = "Hello, World!"),
                         property3 = new_property(class_logical,
                                                  default = TRUE)))

output_object <- function(x, class = S7_object, description = NULL,
                          example = if (identical(class, S7_object))
                              Example() else class(), ...)
{
    stopifnot(is.null(example) || inherits(example, class))
    schema <- as_json_schema(class, description, ...)
    example <- as_json(example)
    format <- JSONFormat(schema = schema, example = example)
    x@binding@output <- format
    x
}
