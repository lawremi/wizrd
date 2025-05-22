input_variables_prop <- new_property(class_character, setter = \(self, value) {
    self@input_variables <- as.character(value)
    self
})

PromptTemplate := new_class(
    properties = list(
        template = scalar(class_character),
        input_variables = input_variables_prop,
        template_format = scalar(class_character)
    )
)

construct_langchain_object <- function(x) {
    constructor <- match.fun(x$id[length(x$id)])
    do.call(constructor, x$kwargs)
}

method(convert, list(class_list, PromptTemplate)) <- function(from, to) {
    construct_langchain_object(from)
}

MessagePromptTemplate := new_class(
    properties = list(
        prompt = new_property(
            PromptTemplate,
            setter = \(self, value) {
                self@prompt <- convert(value, PromptTemplate)
                self
            }
        )
    )
)

SystemMessagePromptTemplate := new_class(MessagePromptTemplate)
HumanMessagePromptTemplate := new_class(MessagePromptTemplate)
AIMessagePromptTemplate := new_class(MessagePromptTemplate)

method(convert, list(class_list, MessagePromptTemplate)) <- function(from, to) {
    construct_langchain_object(from)
}

ChatPromptTemplate := new_class(
    properties = list(
        input_variables = input_variables_prop,
        messages = list_of(
            MessagePromptTemplate,
            setter = \(self, value) {
                self@messages <- lapply(value, convert, MessagePromptTemplate)
                self
            }
        )
    )
)

StructuredPrompt := new_class(
    ChatPromptTemplate,
    properties = list(
        "schema_" = class_list
    )
)

parse_langsmith_id <- function(id) {
    assert_string(id)
    parts <- strsplit(id, "/")[[1L]]
    name_parts <- strsplit(tail(parts, 1L), ":")[[1L]]
    parts[length(parts)] <- name_parts[1L]
    commit <- if (length(name_parts) == 1L) "latest" else name_parts[2L]
    c(parts, commit)
}

parse_langsmith_template <- function(x) {
    construct_langchain_object(x$manifest)
}

pull_langsmith_template <- function(id,
                                    url = "https://api.smith.langchain.com/") {
    parts <- parse_langsmith_id(id)
    httr2::request(url) |>
        httr2::req_url_path_append("commits", parts) |>
        httr2::req_perform() |>
        httr2::resp_body_json(simplifyVector = TRUE,
                              simplifyDataFrame = FALSE) |>
        parse_langsmith_template()
}

method(prompt_as, list(Agent, HubID)) <- function(x, format) {
    prompt_as(x, pull_langsmith_template(format))
}

method(system_prompt_as, list(Agent, HubID)) <- function(x, format) {
    system_prompt_as(x, pull_langsmith_template(format))
}

method(convert, list(PromptTemplate, class_character)) <- function(from, to) {
    ans <- switch(from@template_format, "f-string" = as_glue(from@template),
                  mustache = whisker(from@template))
    if (is.null(ans))
        stop("unsupported template format: ", from@template_format)
    ans
}

method(convert, list(PromptTemplate, TextFormat)) <- function(from, to) {
    convert(convert(from, class_character), to)
}

method(prompt_as, list(Agent, ChatPromptTemplate)) <- function(x, format) {
    for (msg in format@messages) {
        x <- prompt_as(x, msg)
    }
    x
}

method(prompt_as, list(Agent, SystemMessagePromptTemplate)) <-
    function(x, format) {
        system_prompt_as(x, format@prompt)
    }

method(prompt_as, list(Agent, HumanMessagePromptTemplate)) <-
    function(x, format) {
        prompt_as(x, format@prompt)
    }

method(prompt_as, list(Agent, StructuredPrompt)) <- function(x, format) {
    prompt_as(x, super(format, ChatPromptTemplate)) |>
        output_as(format@"schema_" |> norm_json_schema())
}
