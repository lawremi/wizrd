LanguageModelParams <- new_class(
    "LanguageModelParams",
    properties = list(
        temperature = nullable(scalar(class_numeric, min = 0)),
        top_p = nullable(scalar(class_numeric, min = 0, max = 1)),
        top_k = nullable(scalar(class_integer, min = 1L)),
        max_tokens = nullable(scalar(class_integer, min = 0L)),
        presence_penalty = nullable(scalar(class_numeric)),
        frequency_penalty = nullable(scalar(class_numeric)),
        stop = new_property(
            NULL | class_character,
            validator = function(value) {
                if (anyNA(value))
                    "must not contain NAs"
            })
    )
)
