parse_langsmith_id <- function(id) {
    assert_string(id)
    
    parts <- strsplit(id, "/")[[1L]]
    name_parts <- strsplit(tail(parts, 1L), ":")[[1L]]
    parts[length(parts)] <- name_parts[1L]
    commit <- if (length(name_parts) == 1L) "latest" else name_parts[2L]
    c(parts, commit)
}

parse_langsmith_template <- function(x) {
    for(msg in x$manifest$kwargs$messages)
        if ("HumanMessagePromptTemplate" %in% msg$id) {
            prompt <- msg$kwargs$prompt$kwargs
            if (prompt$template_format != "f-string")
                stop("unsupported template format: ", prompt$template_format)
            return(prompt$template)
        }
}

pull_langsmith_template <- function(id, url = "https://api.smith.langchain.com/")
{
    parts <- parse_langsmith_id(id)
    httr2::request(url) |> httr2::req_url_path_append("commits", parts) |>
        httr2::req_perform() |> httr2::resp_body_json() |>
        parse_langsmith_template()
}
