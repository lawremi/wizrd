ResponseConverter <- new_class("ResponseConverter",
                               properties = list(format = ResponseFormat))

ListResponseConverter <- new_class("ListResponseConverter", ResponseConverter,
                                   properties = list(format = JSONFormat))

ObjectResponseConverter <- new_class("ObjectResponseConverter",
                                     ResponseConverter,
                                     properties = list(format = JSONFormat))

DataFrameResponseConverter <- new_class("DataFrameResponseConverter",
                                        ResponseConverter,
                                        properties = list(format = CSVFormat))

as_json <- function(x) {
    prop_as_json <- function(x) {
        if (inherits(x, S7_object))
            as_json(x)
        else x
    }
    .data <- S7_data(x)
    c(.class = S7:::S7_class_name(S7_class(x)),
      if (typeof(.data) != "object") list(.data = .data),
      lapply(props(x), prop_as_json))
}

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
    x@response_converter <- ObjectResponseConverter(format = format)
    x
}

convert_response <- new_generic("convert_response", c("x", "response"))

method(convert_response, c(ResponseConverter, ChatMessage)) <-
    function(x, response)
    {
        set_props(response, object = convert_response(x, response@content))
    }

method(convert_response, c(ResponseConverter, class_any)) <-
    function(x, response) {
        response
    }
