ResponseFormat <- new_class("ResponseFormat",
                            properties = list(example = class_any))

JSONFormat <- new_class("JSONFormat", ResponseFormat,
                        properties = list(schema = class_list,
                                          ## TODO: validate with jsonvalidate
                                          example = class_list))

CSVFormat <- new_class("CSVFormat", ResponseFormat,
                       properties = list(schema = class_list,
                                         example = class_data.frame))

CodeFormat <- new_class("CodeFormat", ResponseFormat)

respond_with_format <- function(x, format = ResponseFormat()) {
    set_props(response_converter = ResponseConverter(format = format))
}

respond_with_json <- function(x, schema = list(), example = list()) {
    respond_with_format(x, JSONFormat(schema = schema, example = example))
}

format_prompt <- new_generic("format_prompt", c("x", "format"))

method(format_prompt, list(LanguageModel, ResponseFormat)) <- function(x, format)
{
    NULL
}

method(format_prompt, list(LanguageModel, JSONFormat)) <- function(x, format) {
    prompt <- "Return only JSON, without any explanation or other text.\n"
    if (length(x@schema) > 0L)
        prompt <- paste0(prompt,
                         "The JSON must conform to the following schema:\n\n",
                         toJSON(x),
                         "\nEnsure the JSON matches this schema exactly.\n")
    if (length(x@example) > 0L)
        prompt <- paste(prompt, "Here is an example:\n\n", toJSON(example))
    prompt
}

respond_with_csv <- function(x, schema = list(), example = data.frame()) {
    respond_with_format(x, CSVFormat(schema = schema, example = example))
}

method(format_prompt, list(LanguageModel, CSVFormat)) <- function(x, format) {
    prompt <- "Return only CSV, without any explanation or other text.\n",
    if (length(x@schema) > 0L)
        prompt <- paste0(prompt,
                         "This JSON schema defines the structure of the data:\n",
                         toJSON(x),
                         "\nInterpret the JSON schema to understand ",
                         "the required columns and data types and produce ",
                         "the corresponding CSV. ",
                         "Ensure the CSV matches this schema exactly.\n")
    if (length(x@example) > 0L)
        prompt <- paste(prompt, "Here is an example:\n\n", toJSON(example))
    prompt
}

respond_with_code <- function(x) {
    respond_with_format(x, CodeFormat())
}
