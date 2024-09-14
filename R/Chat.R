Chat_contents <- function(self) lapply(self@messages, prop, "content")
Chat_objects <- function(self) lapply(self@messages, prop, "object")
Chat_roles <- function(self) vapply(self@messages, prop, character(1L), "role")

Chat <- new_class("Chat",
                  properties = list(
                      model = LanguageModel,
                      messages = new_list_property(of = ChatMessage),
                      contents = new_property(class_list,
                                              getter = Chat_contents),
                      objects = new_property(class_list,
                                             getter = Chat_objects),
                      roles = new_property(class_list,
                                           getter = Chat_roles),
                      env = class_environment
                  ))

method(predict, Chat) <- function(object, input, ...) {
    last_output(chat(object, input, ...))
}

method(chat, Chat) <- function(x, input, ...) {
    if (length(input) == 0L)
        return(x)
    if (!is.list(input) || is.object(input))
        input <- list(input)
    chat(x@model, c(x@messages, input), env = x@env, ...)
}

method(print, Chat) <- function(x, full = FALSE, ...) {
    cat("<Chat>:", length(x@messages), "messages\n")
    if (full) {
        messages <- x@messages
    } else {
        messages <- tail(x@messages, 2L)
        cat(cli::rule(center = "Latest messages", line = " -"))
        cat("\n")
    }
    for(i in seq_along(messages))
        print(messages[[i]])
}

last_message <- function(x, role = NULL) {
    assert_string(role, null.ok = TRUE)
    msg <- if (is.null(role))
               x@messages[[length(x@messages)]]
           else Find(function(m) m@role == role, x@messages, right = TRUE)
    if (is.null(msg))
        stop("No message found",
             if (!is.null(role)) paste0(" for role '", role, "'"))
    msg
}

last_prompt <- function(x) last_message(x, "user")@content
last_input <- function(x) last_message(x, "user")@object

last_response <- function(x) last_message(x, "assistant")@content
last_output <- function(x) last_message(x, "assistant")@object

append_messages <- function(x, ...) {
    set_props(x, messages = c(x@messages, list(...)))
}

backticked_strings <- function(x) {
    cnt <- unlist(Filter(is.character, x@contents[x@roles == "user"]))
    m <- do.call(cbind, regmatches(cnt, regexec("`([^`]*?)`", cnt)))
    unique(matrix(m, 2)[2,])
}

backticked_strings_as_names <- function(args, chat) {
    bt_strs <- backticked_strings(chat)
    lapply(args, \(arg) {
        if (is.character(arg) && length(arg) == 1L && arg %in% bt_strs)
            as.name(arg)
        else arg
    })
}

handle_tool_calls <- function(x) {
    tool_calls <- last_message(x)@tool_calls
    msgs <- lapply(tool_calls, function(tool_call) {
        binding <- x@model@tools[[tool_call@tool_name]]
        args <- props(detextify(tool_call@arguments, binding@io@input)) |>
            backticked_strings_as_names(x)
        value <- do.call(binding@tool, args, envir = x@env)
        ChatMessage(role = "tool", object = value,
                    content = textify(value, binding@io@output),
                    participant = tool_call@id)
    })
    chat(x, msgs)
}

handle_output <- function(x, output) {
    append_messages(x, detextify(output, x@model@io@output)) |>
        handle_tool_calls()
}
