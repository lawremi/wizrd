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
                                S7_property))

CSVFormat <- new_class("CSVFormat", TextFormat,
                       properties = list(
                           col_classes = class_character
                       ))

CodeFormat <- new_class("CodeFormat", TextFormat,
                        properties = list(language = nullable(prop_string)))

GlueFormat <- new_class("GlueFormat", TextFormat,
                        properties = list(template = class_character))

whisker := new_class(ScalarString)

WhiskerFormat := new_class(TextFormat,
                           properties = list(template = whisker))

is_schema_class <- function(x) {
    S7:::class_inherits(x, JSONFormat@properties$schema_class$class)
}

json_format <- function(schema = list())
{
    schema_class <- if (is_schema_class(schema)) schema else schema_class(schema)
    schema <- box_json_schema(as_json_schema(schema))
    JSONFormat(schema = schema, schema_class = schema_class)
}

as_col_classes <- new_generic("as_col_classes", "x")

method(as_col_classes, class_character | class_logical) <- function(x) x

method(as_col_classes, class_data.frame) <- function(x) {
    vapply(x, \(xi) class(xi)[1L], character(1L))
}

csv_format <- function(col_classes = NA)
{
    col_classes <- as_col_classes(col_classes)
    CSVFormat(col_classes = col_classes)
}

code_format <- function(language = "R") {
    CodeFormat(language = language)
}

glue_format <- function(template) {
    GlueFormat(template = template)
}

method(convert, list(class_any, TextFormat)) <- function(from, to) {
    json_format(from)
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

class_glue <- new_S3_class(c("glue", "character"))

method(convert, list(class_glue, TextFormat)) <- function(from, to) {
    GlueFormat(from)
}

method(convert, list(whisker, TextFormat)) <- function(from, to) {
    WhiskerFormat(from)
}

method(convert, list(PlainTextFormat, S7_property)) <- function(from, to) {
    prop_string
}

glue_params <- function(x) {
    regmatches(x, gregexec("\\{(.*?)\\}", x))[[1L]][2L,]
}

method(convert, list(GlueFormat, S7_class)) <- function(from, to) {
    params <- glue_params(from@template)
    props <- setNames(rep(list(class_character), length(params)), params)
    new_class("glue_parameters", Parameters, properties = props)
}

method(convert, list(class_data.frame, S7_property)) <- function(from, to, ...) {
    new_data_frame_property(prototype = from)
}

method(convert, list(JSONFormat, S7_property)) <- function(from, to) {
    convert(from@schema_class, S7_property)
}

method(convert, list(TextFormat, S7_property)) <- function(from, to) {
    convert(convert(from, S7_class), S7_property)
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

method(textify, list(class_data.frame, PlainTextFormat)) <- function(x, format) {
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
            val <- jsonlite::unbox(val)
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
        jsonify(x) |> box_json() |> toJSON(null = "null", POSIXt = "ISO8601") |>
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

## Could this be done with S7?
jsonic <- function(x) structure(x, class = "jsonic")
class_jsonic <- new_S3_class("jsonic")

dejsonify <- new_generic("dejsonify", c("x", "spec"))

method(dejsonify, list(class_list | class_data.frame, S7_class)) <-
    function(x, spec) {
        keep <- intersect(names(x), names(spec@properties))
        do.call(spec, Map(dejsonify, x[keep], spec@properties[keep]),
                quote = TRUE)
    }

method(dejsonify, list(class_any, S7_property)) <- function(x, spec) {
    dejsonify(x, spec$class)
}

method(dejsonify, list(class_any, list_S7_property)) <- function(x, spec) {
    lapply(x, dejsonify, spec$of)
}

method(dejsonify, list(class_data.frame, list_S7_property)) <- function(x, spec)
{
    dejsonify(split(x, seq(nrow(x))), spec)
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
    convert(jsonic(x), spec)
}

method(dejsonify, list(class_any, S7_union)) <- function(x, spec) {
    for(class in spec$classes) {
        ans <- try(dejsonify(x, class), silent = TRUE)
        if (!inherits(ans, "try-error"))
            return(ans)
    }
    stop("failed to convert to ", capture.output(print(spec)))
}

method(dejsonify, list(class_list, class_data.frame)) <- function(x, spec)
{
    do.call(rbind, c(list(spec), x))
}

method(dejsonify, list(class_data.frame, class_data.frame)) <- function(x, spec)
{
    x[colnames(spec)] # JSON schema does not ensure order
}

method(dejsonify, list(class_data.frame, data_frame_S7_property)) <-
    function(x, spec) {
        dejsonify(x, spec$prototype)
    }

method(dejsonify, list(class_any, S7_any)) <- function(x, spec) x

detextify <- new_generic("detextify", c("x", "format"))

unbox_json <- function(x) x$"__boxed" %||% x
is_json_object <- function(x) is.list(x) && !is.null(names(x))
box_json <- function(x) if (!is_json_object(x)) list("__boxed" = x) else x

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
