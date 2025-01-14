OpenAIAPIServer <- new_class("OpenAIAPIServer", LanguageModelServer)

openai_response_models <- function(x) x$data

openai_models_perform <- function(x) {
    server_request(x) |> httr2::req_url_path_append(models_path(x)) |>
        httr2::req_perform() |> httr2::resp_body_json()
}

method(models, OpenAIAPIServer) <- function(x) {
    openai_models_perform(x) |> openai_response_models()
}

openai_body_messages <- function(body, messages) {
    assert_list(messages, "wizrd::ChatMessage")
    put(body, messages = unname(lapply(messages, openai_encode_message)))
}

openai_body_tools <- function(body, tools) {
    assert_list(tools, "wizrd::ToolBinding")
    put(body, tools = if (length(tools) > 0L)
        unname(lapply(tools, openai_encode_tool)))
}

openai_response_format <- new_generic("openai_response_format", "x")

method(openai_response_format, TextFormat) <- function(x) {
   NULL 
}

schema_name_regex <- "^[a-zA-Z0-9_-]+$"

schema_is_strict <- function(x) {
    if (identical(x$type, "object")) {
        identical(x$additionalProperties, FALSE) &&
            all(names(x$properties) %in% x$required) &&
            all(vapply(x$properties, schema_is_strict, logical(1L)))
    } else if (identical(x$type, "array") && !is.null(x$items))
        schema_is_strict(x$items)
    else TRUE
}

method(openai_response_format, JSONFormat) <- function(x) {
    if (identical(x@schema$type, "object")) {
        name <- x@schema$title %||% "object"
        if (!grepl(schema_name_regex, name))
            stop("schema@title must match '", schema_name_regex, "'")
        list(type = "json_schema", json_schema = list(
            name = name,
            schema = x@schema,
            strict = schema_is_strict(x@schema)
        ))
    } else list(type = "json_object")
}

openai_body_response_format <- function(body, output_format) {
    put(body, response_format = openai_response_format(output_format))
}

openai_chat_body <- function(model, messages, tools, output_format, params, ...)
{
    assert_string(model)    
    list(model = model, ...) |> openai_body_messages(messages) |>
        openai_body_params(params) |> openai_body_tools(tools) |>
        openai_body_response_format(output_format)
}

openai_body_params <- function(body, params) {
    put(body, Filter(Negate(is.null), props(params)))
}

req_perform_sse <- function(req, onmessage_callback = NULL,
                            event_callbacks = list(),
                            timeout_sec = Inf, buffer_kb = 64)
{
    callback <- function(bytes) {
        text <- rawToChar(bytes)
        msgs <- strsplit(text, "\n\n", fixed = TRUE)[[1L]]
        m <- gregexec("(event|data|id|retry): ?(.*)", msgs, perl = TRUE)
        for (mat in regmatches(msgs, m)) {
            event <- split(mat[3L,], mat[2L,])
            event$data <- paste(event$data, collapse = "\n")
            if (is.null(event$event)) {
                event$event <- "message"
                callback <- onmessage_callback
            } else callback <- event_callbacks[[event$event]]
            if (!is.null(callback) && !isTRUE(callback(event)))
                return(FALSE)
        }
        TRUE
    }
    round <- function(bytes) {
        nl <- bytes == charToRaw("\n")
        which(diff(nl) == 0L & nl[-1L])
    }
    httr2::req_perform_stream(req, callback, timeout_sec, buffer_kb, round)
}

openai_chat_perform_stream <- function(req, stream_callback) {
    assert_function(stream_callback, nargs = 1L)
    content <- NULL
    sse_callback <- function(event) {
        if (event$data == "[DONE]")
            return(FALSE)
        data <- fromJSON(event$data, simplifyDataFrame = FALSE)
        new_content <- data$choices[[1L]]$delta$content
        content <<- c(content, new_content)
        stream_callback(new_content)
    }
    req_perform_sse(req, sse_callback, buffer_kb = 256 / 1024)
    ChatMessage(role = "assistant", content = paste(content, collapse = ""))
}

method(perform_chat, OpenAIAPIServer) <- function(x, model, messages, tools,
                                                  io, params, stream_callback,
                                                  ...)
{
    openai_chat_body(model, messages, tools, io@output, params,
                     stream = !is.null(stream_callback), ...) |>
        openai_chat_perform(x, stream_callback) |>
        openai_response_chat_message()
}

openai_chat_perform <- function(body, server, stream_callback) {
    req <- server_request(server) |>
        httr2::req_url_path_append(chat_completions_path(server, body$model)) |>
        httr2::req_body_json(body)
    if (!is.null(stream_callback)) {
        req |> openai_chat_perform_stream(stream_callback)
    } else {
        req |> httr2::req_perform() |> httr2::resp_body_json()
    }
}

method(server_req_api_key, OpenAIAPIServer) <- function(server, req, key) {
    httr2::req_auth_bearer_token(req, key)
}

method(server_req_api_version, OpenAIAPIServer) <- function(server, req) {
    httr2::req_url_path_append(req, "v1")
}

method(chat_completions_path, OpenAIAPIServer) <- function(server, model) {
    "chat/completions"
}

method(embeddings_path, OpenAIAPIServer) <- function(server, model) {
    "embeddings"
}

method(models_path, OpenAIAPIServer) <- function(server) {
    "models"
}

openai_message_tool_calls <- function(message) {
    tool_calls <- message$tool_calls
    lapply(tool_calls, function(tool_call) {
        ToolCall(id = tool_call$id, tool_name = tool_call$`function`$name,
                 arguments = fromJSON(tool_call$`function`$arguments))
    })
}

openai_response_chat_message <- function(x) {
    if (inherits(x, ChatMessage)) # captured during streaming
        return(x)
    ## TODO: handle multiple choices (via callback?)
    msg <- x$choices[[1L]]$message
    if (getOption("httr2_verbosity", 0L) == 3L)
        message(toJSON(msg))
    ChatMessage(content = msg$content %||% character(),
                tool_calls = openai_message_tool_calls(msg),
                role = "assistant", refusal = msg$refusal)
}

openai_encode_message <- function(x) {
    content <- openai_encode_content(x@content)
    stopifnot(is.null(content) || test_list(content, names = "unnamed") ||
                  test_string(content))
    message <- list(role = x@role)
    message$content <- content
    if (x@role == "tool")
        message$tool_call_id <- x@participant
    else message$name <- x@participant
    ## recapitulate tool calls for the context
    tool_calls <- lapply(x@tool_calls, function(tool_call) {
        list(id = tool_call@id, type = "function",
             `function` = list(
                 name = tool_call@tool_name,
                 arguments = toJSON(tool_call@arguments, auto_unbox = TRUE)
             ))
    })
    message$tool_calls <- if (length(tool_calls) > 0L) tool_calls
    message
}

openai_encode_content <- new_generic("openai_encode_content", "x")

method(openai_encode_content, class_character) <- function(x) {
    if (length(x) > 1L)
        openai_encode_content(as.list(x))
    else if (length(x) == 1L) x
}

method(openai_encode_content, MediaURI) <- function(x) {
    list(openai_encode_content_part(x))
}

method(openai_encode_content, class_list) <- function(x) {
    lapply(unname(x), openai_encode_content_part)
}

openai_encode_content_part <- new_generic("openai_encode_content_part", "x")

method(openai_encode_content_part, class_character) <- function(x) {
    list(type = "text", text = x)
}

method(openai_encode_content_part, ImageURI) <- function(x) {
    list(type = "image_url", image_url = list(url = x))
}

openai_encode_tool <- new_generic("openai_encode_tool", "x")

method(openai_encode_tool, ToolBinding) <- function(x) {
    stopifnot(inherits(x@io@input, JSONFormat))
    list(type = "function",
         `function` = list(name = x@tool@name,
                           description = describe_tool(x),
                           parameters = x@io@input@schema))
}

openai_embedding_body <- function(model, input, dimensions) {
    c(list(input = as.character(input), model = model),
      dimensions = dimensions)
}

method(perform_embedding, OpenAIAPIServer) <- function(x, model, data,
                                                       ndim = NULL)
{
    assert_string(model)
    assert_integerish(ndim, lower = 1L, null.ok = TRUE)
    
    openai_embedding_body(model, data, ndim) |>
        openai_embedding_perform(x) |>
        openai_response_embedding()
}

openai_embedding_perform <- function(body, server) {
    req <- server_request(server) |>
        httr2::req_url_path_append(embeddings_path(server, body$model)) |>
        httr2::req_body_json(body) |>
        httr2::req_perform() |>
        httr2::resp_body_json(simplifyVector = TRUE, simplifyDataFrame = FALSE)
}

openai_response_embedding <- function(x) {
    do.call(rbind, lapply(x$data, `[[`, "embedding"))
}
