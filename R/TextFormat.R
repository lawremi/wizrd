TextFormat <- new_class("TextFormat", abstract = TRUE)

PlainTextFormat <- new_class("PlainTextFormat", TextFormat)

JSONFormat <- new_class("JSONFormat", TextFormat,
                        properties = list(
                            schema = new_property(
                                class_list,
                                validator = \(value) {
                                    if (!identical(value$type, "object"))
                                        "must specify an object"
                                },
                                default = list(type = "object")
                            ),
                            schema_class = union_classes | class_data.frame |
                                class_S7_property))

CSVFormat <- new_class("CSVFormat", TextFormat,
                       properties = list(
                           col_classes = class_character
                       ))

CodeFormat <- new_class("CodeFormat", TextFormat,
                        properties = list(
                            language = nullable(scalar(class_character))
                        ))

GlueFormat <- new_class("GlueFormat", TextFormat,
                        properties = list(template = class_character))

whisker := new_class(ScalarString)

WhiskerFormat := new_class(TextFormat,
                           properties = list(template = whisker))

FunctionFormat := new_class(TextFormat,
                            properties = list(fun = class_function))

is_schema_class <- function(x) {
    S7_inherits(x, JSONFormat@properties$schema_class$class)
}

json_format <- function(schema = list(), ...) {
    schema_class <- if (is_schema_class(schema)) {
        schema
    } else {
        schema_class(schema)
    }
    schema <- box_json_schema(as_json_schema(schema, ...))
    JSONFormat(schema = schema, schema_class = schema_class)
}

as_col_classes <- new_generic("as_col_classes", "x")

method(as_col_classes, class_character | class_logical) <- function(x) x

method(as_col_classes, class_data.frame) <- function(x) {
    vapply(x, \(xi) class(xi)[1L], character(1L))
}

csv_format <- function(col_classes = NA) {
    col_classes <- as_col_classes(col_classes)
    CSVFormat(col_classes = col_classes)
}

code_format <- function(language = "R") {
    CodeFormat(language = language)
}

glue_format <- function(template) {
    GlueFormat(template = template)
}

method(convert, list(class_any, TextFormat)) <- function(from, to, ...) {
    json_format(from, ...)
}

method(convert, list(TextFormat, TextFormat)) <- function(from, to) {
    from
}

method(convert, list(File, TextFormat)) <- function(from, to) {
    convert(as_glue(read_as_string(from)), to)
}

method(convert, list(HubID, TextFormat)) <- function(from, to) {
    convert(pull_langsmith_template(from), to)
}

method(convert, list(class_function, TextFormat)) <- function(from, to) {
    FunctionFormat(fun = from)
}

class_glue <- new_S3_class(c("glue", "character"))

method(convert, list(class_glue, TextFormat)) <- function(from, to) {
    GlueFormat(from)
}

method(convert, list(whisker, TextFormat)) <- function(from, to) {
    WhiskerFormat(from)
}

method(convert, list(PlainTextFormat, class_S7_property)) <-
    function(from, to) {
        scalar(class_character)
    }

glue_params <- function(x) {
    regmatches(x, gregexec("\\{(.*?)\\}", x))[[1L]][2L, ]
}

method(convert, list(GlueFormat, class_S7_class)) <- function(from, to) {
    params <- glue_params(from@template)
    props <- setNames(rep(list(class_character), length(params)), params)
    new_class("glue_parameters", Parameters, properties = props)
}

method(convert, list(class_data.frame, class_S7_property)) <- function(from, to,
                                                                       ...) {
    data_frame_of(prototype = from)
}

method(convert, list(JSONFormat, class_S7_property)) <- function(from, to) {
    convert(from@schema_class, class_S7_property)
}

method(convert, list(TextFormat, class_S7_property)) <- function(from, to) {
    convert(convert(from, class_S7_class), class_S7_property)
}

textify <- new_generic("textify", c("x", "format"))

method(textify, list(class_any, class_missing)) <- function(x, format) {
    textify(x, PlainTextFormat())
}

method(textify, list(class_any, PlainTextFormat)) <- function(x, format) {
    if (!is.object(x) && length(x) == 1L)
        unname(as.character(x))
    else textify(x, JSONFormat())
}

method(textify, list(class_character, PlainTextFormat)) <- function(x, format) {
    unname(x)
}

method(textify, list(class_list, PlainTextFormat)) <- function(x, format) {
    if (is.object(names(x)))
        textify(x, JSONFormat())
    else lapply(x, textify, format)
}

method(textify, list(class_json, PlainTextFormat)) <-
    function(x, format) unclass(x)

method(textify, list(class_data.frame, PlainTextFormat)) <- function(x,
                                                                     format) {
    con <- file()
    on.exit(close(con))
    write.csv(x, con, row.names = FALSE)
    read_as_string(con)
}

nativeRaster <- new_S3_class("nativeRaster")
raster <- new_S3_class("raster")
union_raster <- new_union(nativeRaster, raster)

method(textify, list(union_raster, PlainTextFormat)) <- function(x, format) {
    convert(x, MediaURI)
}

method(textify, list(class_list | class_any | class_data.frame, JSONFormat)) <-
    function(x, format) {
        jsonify(x) |>
            box_json() |>
            toJSON(null = "null", POSIXt = "ISO8601") |>
            unclass()
    }

template_contexts <- class_list | class_data.frame | class_any

method(textify, list(template_contexts, GlueFormat)) <- function(x, format) {
    if (is.character(x))
        x <- list(input = x)
    glue(format@template, .envir = as.environment(x))
}

method(textify, list(template_contexts, WhiskerFormat)) <- function(x, format) {
    require_ns("whisker", "instantiate whisker templates")
    if (is.character(x))
        x <- list(input = x)
    whisker::whisker.render(format@template, as.environment(x))
}

method(textify, list(template_contexts, FunctionFormat)) <- function(x,
                                                                     format) {
    do.call(format@fun, x)
}

detextify <- new_generic("detextify", c("x", "format"))

method(detextify, list(class_character, JSONFormat)) <- function(x, format) {
    if (length(x) != 1L) # could make simplification optional flag on format
        lapply(x, detextify, format)
    else fromJSON(x) |> unbox_json() |> dejsonify(format@schema_class)
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
    read.csv(text = x, colClasses = format@col_classes,
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
