OpenAIAPIServer <- new_class("OpenAIAPIServer", LanguageModelServer)

OpenAIAPIResponse <- new_class("OpenAIAPIResponse", class_list)

openai_body_messages <- function(messages) {
    assert_messages(messages)
    lapply(messages, openai_encode_message)
}

openai_body_tools <- function(tools) {
    assert_list(tools, "function")
    lapply(tools, openai_encode_tool)
}

openai_response_format <- new_generic("openai_response_format",
                                      "response_format")

method(openai_response_format, ResponseFormat) <- function(response_format) {
   NULL 
}

method(openai_response_format, JSONResponseFormat) <- function(response_format) {
    if (length(response_format@json_schema) > 0L)
        list(type = "json_schema", json_schema = response_format@json_schema)
    else list(type = "json_object")
}

openai_req_body_chat <- function(req, model, messages, tools, output_format, ...)
{
    assert_string(model)
    
    body <- list(model = model, messages = openai_body_messages(messages),
                 tools = openai_body_tools(tools), ...)
    body$response_format <- openai_response_format(output_format)
    
    req |> httr2::req_body_json(body)
}

req_perform_sse <- function(req, event_callbacks = list(),
                            onmessage_callback = NULL,
                            timeout_sec = Inf, buffer_kb = 64)
{
    callback <- function(bytes) {
        text <- rawToChar(bytes)
        msgs <- strsplit(text, "\n\n", fixed = TRUE)[[1L]]
        m <- gregexec("(event|data|id|retry):?(.*)", msgs, perl = TRUE)
        for (mat in regmatches(msgs, m)) {
            event <- split(mat[3L,], mat[2L,])
            event$data <- paste(event$data, collapse = "\n")
            if (is.null(event$event))
                callback <- onmessage_callback
            else callback <- event_callbacks[[event$event]]
            if (!is.null(callback) && !isTRUE(callback(event)))
                return(FALSE)
        }
        TRUE
    }
    round <- function(bytes) {
        which(diff(bytes == charToRaw("\n")) == 0L)
    }
    req_perform_stream(req, callback, timeout_sec, buffer_kb, round)
}

req_capture_stream_openai <- function(req, stream_callback) {
    assert_function(stream_callback, nargs = 1L)
    content <- NULL
    sse_callback <- function(event) {
        data <- fromJSON(event$data)
        new_content <- data$choices[[1L]]$delta$content
        content <<- c(content, new_content)
        if (!is.null(new_content))
            stream_callback(new_content)
    }
    req_perform_sse(req, sse_callback)
    ChatMessage(content = paste(content, collapse = ""))
}

method(chat, OpenAIAPIServer) <- function(object, model, messages, tools,
                                          output_format, stream_callback, ...)
{
    req <- create_request(object) |>
        httr2::req_url_path_append("v1", "chat", "completions") |>
        openai_req_body_chat(model, messages, tools, output_format,
                             stream = !is.null(stream_handler), ...)
    if (!is.null(stream_handler)) {
        req |> req_capture_stream_openai(stream_callback)
    } else {
        req |> httr2::req_perform() |> httr2::resp_body_json() |>
            OpenAIAPIResponse()
    }
}

method(req_auth_fun, OpenAIAPIServer) <- function(server) {
    httr2::req_auth_bearer_token
}

openai_tool_calls <- function(message) {
    tool_calls <- message$tool_calls
    lapply(tool_calls, function(tool_call) {
        list(id = tool_call$id, tool_name = tool_call$function$name,
             arguments = fromJSON(tool_call$function$arguments))
    })
}

chat_message <- new_generic("chat_message", "x")

method(chat_message, OpenAIAPIResponse) <- function(x) {
    ## TODO: handle multiple choices (via callback?)
    msg <- response$choices[[1L]]
    ChatMessage(content = msg$content, tool_calls = openai_tool_calls(msg))
}

openai_encode_message <- function(x) {
    tool_calls <- lapply(x@tool_calls, function(tool_call) {
        list(id = tool_call@id, type = "function",
             function = list(name = tool_call@tool_name, tool_call@arguments))
    })
    list(role = x@role, content = openai_encode_content(x@content),
         tool_calls = tool_calls)
}

openai_encode_content <- new_generic("openai_encode_content", "x")

method(openai_encode_content, class_character) <- identity

method(openai_encode_content, union_raster) <- function(x) {
    list(openai_encode_content_part(x))
}

method(openai_encode_content, class_list) <- function(x) {
    lapply(x, openai_encode_content_part)
}

openai_encode_content_part <- new_generic("openai_encode_content_part", "x")

method(openai_encode_content_part, class_character) <- function(x) {
    list(type = "text", text = x)
}

method(openai_encode_content_part, union_raster) <- function(x) {
    list(type = "image_url", image_url = list(url = image_data_uri(x)))
}

method(openai_encode_content_part, class_list) <- function(x) {
    list(type = "text", text = toJSON(x))
}

openai_encode_tool <- new_generic("openai_encode_tool", "x")

method(openai_encode_tool, class_function) <- function(x) {
    openai_encode_tool(Tool_from_function(x, name = NULL))
}

method(openai_encode_tool, Tool) <- function(x) {
    list(type = "function",
         function = list(name = x@name, c(description = x@description),
                         parameters = arg_schema))
}
