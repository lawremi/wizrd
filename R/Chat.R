Chat_contents <- function(self) lapply(self@messages, `@`, "content")
Chat_objects <- function(self) lapply(self@messages, `@`, "object")
Chat_roles <- function(self) vapply(self@messages, `@`, character(1L), "role")

Chat <- new_class("Chat",
                  properties = list(
                      model = LanguageModel,
                      messages = new_list_property(of = ChatMessage),
                      contents = new_property(class_list,
                                              getter = Chat_contents),
                      objects = new_property(class_list,
                                             getter = Chat_objects),
                      roles = new_property(class_list,
                                           getter = Chat_roles)
                  ))

normalize_messages <- function(x) {
    set_slots(messages = lapply(x@messages, normalize_chat_message))
}

method(predict, Chat) <- function(object, input, ...) {
    last_message(chat(object, input, ...))
}

method(chat, Chat) <- function(object, input, ...) {
    assert_string(input)
    chat(object@model, c(object@messages, list(input)), ...)
}

method(print, Chat) <- function(x, full = FALSE, ...) {
    cat("<Chat>:", length(x@messages), "messages\n")
    if (full) {
        messages <- x@messages
        cli::cli_h3("Transcript")
    } else {
        messages <- tail(x@messages, 2L)
        cli::cli_h3("Latest messages")
    }
    for(i in seq_along(messages))
        print(messages[[i]])
}

last_message <- function(x, role = NULL) {
    assert_string(role, null.ok = TRUE)
    if (is.null(role))
        x@messages[[length(x@messages)]]
    else Find(function(m) m@role == role, x@messages, right = TRUE)
}

last_prompt <- function(x) last_message(x, "user")@content
last_input <- function(x) last_message(x, "user")@object

last_response <- function(x) last_message(x, "assistant")@content
last_output <- function(x) last_message(x, "assistant")@object

append_messages <- function(x, ...) {
    set_props(messages = c(x@messages, list(...)))
}

append_output <- function(x, output) {
    output <- convert_output(x@model@output_converter, output)
    apppend_messages(x, output)
}

handle_tool_calls <- function(x) {
    tool_calls <- last_message(x)@tool_calls
    tool_content <- lapply(tool_calls, function(tool_call) {
        do.call(x@model@tools[[tool_call@tool_name]], tool_call@parameters)
    })
    chat(x, list(tool = tool_content))
}

handle_output <- function(x, output) {
    append_output(x, output) |> handle_tool_calls()
}
