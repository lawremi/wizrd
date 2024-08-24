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

method(predict, Chat) <- function(object, input, ...) {
    last_message(chat(object, input, ...))
}

method(chat, Chat) <- function(x, input, ...) {
    if (length(input) == 0L)
        return(x)
    if (!is.list(input) || is.object(input))
        input <- list(input)
    chat(x@model, c(x@messages, input), ...)
}

method(str, Chat) <- function(object, full = FALSE, ...) {
    cat("<Chat>:", length(object@messages), "messages\n")
    if (full) {
        messages <- object@messages
        cli::cli_h3("Transcript")
    } else {
        messages <- tail(object@messages, 2L)
        cli::cli_h3("Latest messages")
    }
    for(i in seq_along(messages))
        str(messages[[i]])
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
    set_props(x, messages = c(x@messages, list(...)))
}

handle_tool_calls <- function(x) {
    tool_calls <- last_message(x)@tool_calls
    msgs <- lapply(tool_calls, function(tool_call) {
        tool <- x@model@tools[[tool_call@tool_name]]
        value <- do.call(tool, detextify(tool_call@parameters,
                                           tool@io@input))
        ChatMessage(role = "tool", object = value,
                    content = textify(value, tool@io@output))
    })
    chat(x, msgs)
}

handle_output <- function(x, output) {
    append_messages(x, detextify(output, x@model@io@output)) |>
        handle_tool_calls()
}
