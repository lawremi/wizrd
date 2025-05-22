Chat_contents <- function(self) lapply(self@messages, prop, "content")
Chat_objects <- function(self) lapply(self@messages, prop, "object")
Chat_roles <- function(self) vapply(self@messages, prop, character(1L), "role")

Chat <- new_class("Chat",
                  properties = list(
                      model = Agent | ChatPipeline,
                      messages = list_of(ChatMessage),
                      contents = new_property(class_list,
                                              getter = Chat_contents),
                      objects = new_property(class_list,
                                             getter = Chat_objects),
                      roles = new_property(class_list,
                                           getter = Chat_roles),
                      env = new_property(
                          class_environment,
                          default = quote(parent.frame())
                      )
                  ))

method(convert, list(Agent, Chat)) <- function(from, to,
                                               messages = list(),
                                               system_params = list(),
                                               ...) {
    system_prompt <- textify_system_prompt(from, system_params)
    system_msg <- ChatMessage(role = "system", content = system_prompt)
    Chat(model = from, messages = c(system_msg, messages), ...)
}

method(predict, Chat) <- function(object, input, ...) {
    last_output(chat(object, input, ...))
}

is_context <- function(x) is.list(x) && inherits(x[[1L]], ChatMessage)

norm_input <- function(x, chat) {
    if (inherits(x, ChatMessage) && x@role != "user")
        return(x)
    text <- textify(x, chat)
    if (is_context(text))
        text
    else convert(text, ChatMessage)
}

append_input <- function(chat, input) {
    stopifnot(length(input) > 0L)
    if (!is_context(input))
        input <- list(input)
    msgs <- lapply(input, norm_input, chat)
    chat@messages <- c(chat@messages, msgs, recursive = TRUE)
    chat
}

method(chat, Chat) <- function(x, input = NULL, stream_callback = NULL, ...) {
    if (length(input) == 0L)
        return(x)
    x <- append_input(x, input)
    output <- perform_chat(x@model, x@messages, stream_callback, x@env, ...)
    handle_output(x, output)
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
    for (i in seq_along(messages)) {
        print(messages[[i]])
    }
}

last_message <- function(x, role = NULL) {
    assert_string(role, null.ok = TRUE)
    msg <- if (is.null(role)) {
        x@messages[[length(x@messages)]]
    }  else {
        Find(function(m) m@role == role, x@messages, right = TRUE)
    }
    if (is.null(msg))
        stop("No message found",
             if (!is.null(role)) paste0(" for role '", role, "'"))
    msg
}

last_prompt <- function(x) last_message(x, "user")
last_input <- function(x) last_prompt(x)@object

last_response <- function(x) last_message(x, "assistant")
last_output <- function(x) {
    msg <- last_response(x)
    if (!is.null(msg@refusal))
        stop("model refused to generate requested output: ", msg@refusal)
    msg@object
}

append_messages <- function(x, ...) {
    set_props(x, messages = c(x@messages, list(...)))
}

backticked_strings <- function(x) {
    cnt <- unlist(Filter(is.character, x@contents[x@roles == "user"]))
    m <- do.call(cbind, regmatches(cnt, regexec("`([^`]*?)`", cnt)))
    unique(matrix(m, 2)[2, ])
}

backticked_strings_as_names <- function(args, chat) {
    bt_strs <- backticked_strings(chat)
    lapply(args, \(arg) {
        if (is.character(arg) && length(arg) == 1L && arg %in% bt_strs)
            as.name(arg)
        else arg
    })
}

expand_dots <- function(args) {
    args <- c(args, args$`_dots`)
    args$`_dots` <- NULL
    args
}

handle_tool_calls <- function(x) {
    tool_calls <- last_message(x)@tool_calls
    msgs <- lapply(tool_calls, function(tool_call) {
        binding <- x@model@tools[[tool_call@tool_name]]
        args <- props(detextify(tool_call@arguments, binding@io@input)) |>
            backticked_strings_as_names(x) |>
            expand_dots()
        value <- do.call(binding@tool, args, envir = x@env)
        ChatMessage(role = "tool", object = value,
                    content = textify(value, binding@io@output),
                    participant = tool_call@id)
    })
    chat(x, msgs)
}

handle_output := new_generic(c("x", "output"))

method(handle_output, list(Chat, class_any)) <- function(x, output) {
    append_messages(x, detextify(output, x@model)) |>
        handle_tool_calls()
}

method(handle_output, list(Chat, Chat)) <- function(x, output) {
    set_props(x, model = output@model, messages = output@messages)
}

readline_chat <- function(model, env = parent.frame()) {
    ctx <- chat(model, env = env)
    cat("Enter 'Q' to exit.\n")
    while (TRUE) {
        prompt <- readline("User: ")
        if (prompt == "Q")
            break
        ctx <- chat(ctx, prompt)
        print(last_response(ctx))
    }
    invisible(ctx)
}

method(c, Chat) <- c.Chat <- c.ChatPipeline

method(textify, list(class_any, Chat)) <- function(x, format) {
    textify(x, format@model)
}
