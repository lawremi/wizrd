TextFormat <- new_class("TextFormat",
                        properties = list(
                            examples = class_list
                        ))

PlainTextFormat <- new_class("PlainTextFormat", TextFormat)

JSONFormat <- new_class("JSONFormat", PlainTextFormat,
                        properties = list(schema = class_list,
                                          schema_class = union_classes))

CSVFormat <- new_class("CSVFormat", PlainTextFormat,
                       properties = list(
                           col_classes = class_character,
                           examples = new_list_property(
                               of = class_data.frame,
                           )))

CodeFormat <- new_class("CodeFormat", PlainTextFormat,
                        properties = list(language = nullable(prop_string)))

json_format <- function(schema = list(), examples = list())
{
    schema_class <- if (S7:::class_inherits(schema, union_classes)) schema
                    else class_any
    schema <- as_json_schema(schema)
    examples <- lapply(examples, jsonify)
    JSONFormat(schema = schema, schema_class = schema_class,
               examples = examples)
}

as_col_classes <- new_generic("as_col_classes", "x")

method(as_col_classes, class_character | class_logical) <- function(x) x

method(as_col_classes, class_data.frame) <- function(x) {
    vapply(x, \(xi) class(xi)[1L], character(1L))
}

csv_format <- function(col_classes = NA, examples = list())
{
    col_classes <- as_col_classes(col_classes)
    examples <- lapply(examples, as.data.frame)
    CSVFormat(col_classes = col_classes, examples = examples)
}

code_format <- function(language = "R") {
    CodeFormat(language = language)
}

expect_format <- function(x, format = TextFormat()) {
    x@io@input <- format
    x
}

expect_json <- function(x, schema = list(), examples = list())
{
    expect_format(x, json_format(schema, examples))
}

expect_csv <- function(x, col_classes = NA, examples = list())
{
    expect_format(x, csv_format(col_classes, examples))
}

expect_code <- function(x, language = "R") {
    expect_format(x, code_format(language))
}

respond_with_format <- function(x, format = TextFormat()) {
    x@io@output <- format
    x
}

respond_with_json <- function(x, schema = list(), examples = list())
{
    respond_with_format(x, json_format(schema, examples))
}

respond_with_csv <- function(x, col_classes = NA, examples = list())
{
    respond_with_format(x, csv_format(col_classes, examples))
}

respond_with_code <- function(x, language = "R") {
    respond_with_format(x, code_format(language))
}

format_constructor <- new_generic("format_constructor", "x")

method(format_constructor, class_any) <- function(x) json_format

method(format_constructor, class_data.frame) <- function(x) csv_format

output_as <- function(x, schema, examples = list()) {
    respond_with_format(x, format_constructor(schema)(schema, examples))
}

textify <- new_generic("textify", c("x", "format"))

method(textify, list(class_any, TextFormat)) <- function(x, format) {
    textify(x, JSONFormat())
}

method(textify, list(class_character, TextFormat)) <- function(x, format) {
    unname(x)
}

method(textify, list(class_list, TextFormat)) <- function(x, format) {
    lapply(unname(x), textify, format)
}

method(textify, list(class_json, TextFormat)) <- function(x, format) unclass(x)

method(textify, list(class_data.frame, TextFormat)) <- function(x, format) {
    con <- file()
    on.exit(close(con))
    utils::write.csv(x, con, row.names = FALSE)
    paste(readLines(con), collapse = "\n")
}

nativeRaster <- new_S3_class("nativeRaster")
raster <- new_S3_class("raster")
union_raster <- new_union(nativeRaster, raster)

method(textify, list(union_raster, TextFormat)) <- function(x, format) x

jsonify <- new_generic("jsonify", "x")

method(jsonify, class_any) <- identity

method(jsonify, class_expression | class_list) <- function(x) lapply(x, jsonify)

method(jsonify, class_environment) <- function(x) jsonify(as.list(x))

method(jsonify, class_name | class_call | class_formula) <-
    function(x) deparse(x)

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
    c(if (typeof(.data) != "object") list(.data = .data),
      lapply(S7_class(x)@properties, prop_jsonify))
}

method(textify, list(class_list | class_any | class_data.frame, JSONFormat)) <-
    function(x, format)
    {
        unclass(toJSON(jsonify(x), null = "null", POSIXt = "ISO8601"))
    }

## Could this be done with S7?
jsonic <- function(x) structure(x, class = "jsonic")
class_jsonic <- new_S3_class("jsonic")

dejsonify <- new_generic("dejsonify", c("x", "spec"))

method(dejsonify, list(class_list, S7_class)) <- function(x, spec) {
    keep <- intersect(names(x), names(spec@properties))
    do.call(spec, Map(dejsonify, x[keep], spec@properties[keep]), quote = TRUE)
}

method(dejsonify, list(class_any, S7_property)) <- function(x, spec) {
    dejsonify(x, spec$class)
}

method(dejsonify, list(class_any, list_S7_property)) <- function(x, spec) {
    lapply(x, dejsonify, spec$of)
}

method(convert, list(class_jsonic, class_raw)) <- function(from, to) {
    require_ns("base64enc", "decode raw vectors from JSON")
    base64enc::base64decode(from)
}

method(convert, list(class_jsonic, class_function)) <- function(from, to) {
    p <- parse(text=from)[[1L]]
    as.function(c(p[[2L]], p[[3L]]), .GlobalEnv)
}

method(convert, list(class_jsonic, class_call)) <- function(from, to) {
    parse(text=from)[[1L]]
}

method(dejsonify, list(class_any, S7_S3_class | S7_base_class)) <- function(x,
                                                                            spec)
{
    ## cannot target unions with convert()
    if (identical(spec, class_numeric))
        return(as.numeric(x))
    if (identical(spec, class_atomic))
        return(unlist(x))
    if (identical(spec, class_vector))
        return(as.vector(x))
    if (identical(spec, class_language))
        return(parse(text=x)[[1L]])
    convert(json(x), spec)
}

method(dejsonify, list(class_any, S7_union)) <- function(x, spec) {
    for(class in spec$classes) {
        ans <- try(dejsonify(x, class), silent = TRUE)
        if (!inherits(ans, "try-error"))
            return(ans)
    }
    stop("failed to convert to ", capture.output(print(x)))
}

method(dejsonify, list(class_any, S7_any)) <- function(x, spec) x

detextify <- new_generic("detextify", c("x", "format"))

method(detextify, list(class_character, JSONFormat)) <- function(x, format) {
    detextify(fromJSON(x), format)
}

method(detextify, list(class_list, JSONFormat)) <- function(x, format) {
    dejsonify(x, format@schema_class)
}

has_header <- function(x, col_names) {
    identical(gsub("\"", "", sub("\n.*", "", x), fixed = TRUE),
              paste(col_names, collapse = ","))
}

method(detextify, list(class_character, CSVFormat)) <- function(x, format) {
    col_names <- names(format@col_classes)
    header <- has_header(x, col_names)
    if (!header) warning("missing header")
    utils::read.csv(text = x, colClasses = format@col_classes,
                    col.names = col_names, header = header)
}

method(detextify, list(class_any, TextFormat)) <- function(x, format) x

method(textify, list(class_any, CodeFormat)) <- function(x, format) {
    paste(deparse(x), collapse = "\n")
}

method(textify, list(class_expression, CodeFormat)) <- function(x, format) {
    paste(as.character(x), collapse = "\n")
}

method(detextify, list(class_character, CodeFormat)) <- function(x, format) {
    if (format@language == "R")
        parse(text = x)
    else x
}
