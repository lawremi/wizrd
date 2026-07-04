zip <- function(...) {
    Map(list, ...)
}

## Base R candidate
vswitch <- function(EXPR, ...) {
    stopifnot(is.null(EXPR) || is.atomic(EXPR))
    dots <- list(...)
    dot_names <- head(names(dots), -1L)
    if (length(dots) > 1L &&
            (is.null(dot_names) || !all(nzchar(dot_names))))
        stop("all arguments in '...' except for the last must be named")
    if (!all(lengths(dots) == 1L))
        stop("all arguments in '...' must be length one")
    if (anyDuplicated(names(dots)))
        stop("all arguments in '...' must have unique names")

    cases <- c(...)
    if (is.null(cases))
        cases <- logical()
    ans <- cases[as.character(EXPR)]

    notfound <- is.na(names(ans)) & !is.na(EXPR)
    ans[notfound] <- if (identical(names(dots)[length(dots)], "")) {
        dots[[length(dots)]]
    } else {
        EXPR[notfound]
    }

    ans
}

make_args <- function(...) {
    args <- Filter(Negate(is.null), list(...))
    if (length(args) == 0L)
        return(character())
    if (is.null(names(args)))
        names(args) <- rep("", length(args))
    args <- args[!vapply(args, identical, logical(1L), FALSE)]
    prefix <- vswitch(nchar(names(args)), `0` = "", `1` = "-", "--")
    names(args) <- paste0(prefix, sub("_", "-", names(args)))
    flags <- vapply(args, isTRUE, logical(1L))
    ans <- unlist(ifelse(flags, names(args), zip(names(args), args)),
                  use.names = FALSE)
    ans[nzchar(ans)]
}

get_api_key <- function(prefix) {
    key_name <- paste0(prefix, "_API_KEY")
    key <- Sys.getenv(key_name)
    if (key == "")
        stop("$", key_name, " is not set")
    key
}

get_Rd <- function(topic, package = NULL) {
    files <- help(topic, identity(package))
    if (length(files) > 1L)
        stop("Multiple help files found for ", topic, ". Specify the package.")
    if (length(files) == 0L)
        return(NULL)
    filename <- paste0(basename(files), ".Rd")
    package <- basename(dirname(dirname(files)))
    tools::Rd_db(package)[[filename]]
}

Rd_for_tag <- function(Rd, tag) {
    tagstr <- paste0("\\", tag)
    Find(\(x) attr(x, "Rd_tag") == tagstr, Rd)
}

Rd_args <- function(Rd) {
    Rd_parse_args(Rd_for_tag(Rd, "arguments"))
}

Rd_src <- function(Rd) {
    src <- if (is.list(Rd)) {
        vapply(Rd, Rd_src, character(1L))
    } else {
        Rd
    }
    src <- paste(src, collapse = "")
    tag <- attr(Rd, "Rd_tag")
    if (!is.null(tag) && startsWith(tag, "\\"))
        src <- paste0(tag, "{", src, "}")
    src
}

Rd_parse_args <- function(args) {
    items <- Filter(\(x) attr(x, "Rd_tag") == "\\item", args)
    ans <- vapply(items, \(x) Rd_src(x[[2L]]), character(1L))
    ans_names <- vapply(items, \(x) {
        gsub("\\dots{}", "...", Rd_src(x[[1L]]), fixed = TRUE)
    }, character(1L))
    ans_names_split <- strsplit(ans_names, ",", fixed = TRUE)
    setNames(rep(ans, lengths(ans_names_split)),
             trimws(unlist(ans_names_split)))
}

untag <- function(x) {
    attr(x, "Rd_tag") <- NULL
    x
}

Rd_description <- function(Rd) {
    Rd_for_tag(Rd, "description") |> untag() |> Rd_src() |> trimws()
}

Rd_value <- function(Rd) {
    Rd_for_tag(Rd, "value") |> untag() |> Rd_src() |> trimws()
}

Rd_for_function <- function(FUN, name = deparse(substitute(FUN))) {
    if (isNamespace(environment(FUN))) {
        package <- getNamespaceName(environment(FUN))
        Rd <- tryCatch(get_Rd(name, package), error = NULL)
        Rd %||% {
            name <- find_name(FUN, environment(FUN))
            if (!is.null(name))
                get_Rd(name, package)
        }
    }
}

find_name <- function(what, env) {
    objs <- as.list(env)
    pos <- Position(\(x) identical(x, what), objs)
    if (!is.na(pos))
        names(objs)[pos]
}

`:=` <- function(x, y) {
    sym <- substitute(x)
    stopifnot(is.name(sym))
    call <- substitute(y)
    stopifnot(is.call(call))

    nm <- deparse(sym)
    call$name <- nm

    assign(nm, eval(call, parent.frame()), parent.frame())
}

static_inherits <- function(x, parent) {
    while (!is.null(x) && !identical(x@parent, parent))
        x <- x@parent
    !is.null(x)
}

S7_class_with_parent <- function(parent, validator = NULL, ...) {
    new_property(class_S7_class, validator = function(value) {
        c(if (!static_inherits(value, parent))
              paste("must be a descendant of", parent@name),
          if (!is.null(validator))
              validator(value))
    }, ...)
}

union_classes <- class_S7_any | NULL | class_S7_class | class_S7_base_class |
    class_S7_union | class_S7_S3_class |
    methods::getClass("classRepresentation")

norm_json_schema <- function(x) {
    if (is.list(x) && !is.null(x$required)) {
        x$required <- I(x$required)
        lapply(x, norm_json_schema)
    }
    x
}

require_ns <- function(x, to, version = NULL) {
    assert_character(x)
    assert_string(to)

    loaded <- vapply(x, requireNamespace, logical(1L), quietly = TRUE)
    if (any(!loaded))
        stop("Install package", if (sum(loaded) > 1L) "s" else "", " ",
             paste0("'", x[!loaded], "'", collapse = " and "), " to ", to, ".",
             call. = FALSE)

    if (!is.null(version)) {
        outdated <- mapply(compareVersion, version,
                           lapply(x, getNamespaceVersion)) > 0L
        if (any(outdated))
            stop("Update package", if (sum(loaded) > 1L) "s" else "", " ",
                 paste0("'", x[outdated], "'", collapse = " and "),
                 " to ", to, ".", call. = FALSE)
    }
    
    invisible(TRUE)
}

S3_connection <- new_S3_class("connection")

S3_processx_connection <- new_S3_class("processx_connection")
S3_process <- new_S3_class("process")

union_connection <- S3_connection | S3_processx_connection

S3_httr2_response <- new_S3_class("httr2_response")
S3_httr2_request <- new_S3_class("httr2_request")

read_lines := new_generic("con")

method(read_lines, S3_connection) <- function(con, ...) readLines(con, ...)
method(read_lines, S3_processx_connection) <- function(con, ...) {
    processx::conn_read_lines(con, ...)
}

write_lines := new_generic("con", function(con, text, ...) S7_dispatch())

method(write_lines, S3_connection) <- function(con, text, ...) {
    writeLines(text, con, ...)
    flush(con)
}
method(write_lines, S3_processx_connection) <- function(con, text, ...) {
    if (length(text) > 0L)
        text[length(text)] <- paste0(text[length(text)], "\n")
    while (length(text) > 0L)
        text <- processx::conn_write(con, text, ...)
}

Pipe := new_class(
    properties = list(
        process = S3_process,
        stdin = new_property(
            S3_processx_connection,
            getter = function(self) self@process$get_input_connection()
        ),
        stdout = new_property(
            S3_processx_connection,
            getter = function(self) self@process$get_output_connection()
        )
    )
)

method(print, Pipe) <- function(x, ...) {
    cat(S7_class_desc(class(x)), ""); print(x@process)
}

pipex <- function(command, args) {
    verbose_message("running: ", command, " ", paste(args, collapse = " "))
    verbose <- getOption("wizrd_verbose", FALSE)
    processx::process$new(command, args, stdin = "|", stdout = "|",
                          stderr = if (verbose) "") |> Pipe()
}

init_process <- function(path, args, ready_callback, error_callback,
                         poll_timeout = 10L) {
    p <- processx::process$new(path, args, stdout = "|", stderr = "|")

    error <- ""
    output <- ""
    while (p$is_alive()) {
        io <- p$poll_io(poll_timeout)
        if (io["output"] == "ready") {
            output <- paste0(output, p$read_output())
            if (isTRUE(ready_callback(output)))
                return(p)
        }
        if (io["error"] == "ready") { # non-error messages can be sent to stderr
            error <- paste0(error, p$read_error())
            if (isTRUE(ready_callback(error)))
                return(p)
        }
    }

    if (p$get_exit_status() > 0L) {
        stop("Failed to run ", path, ": ",
             error_callback(paste0(error, p$read_error())))
    }

    p
}

assert_port <- function(port) {
    assert_integerish(port, lower = 1024L, upper = 65535L)
}

resp_await_sse <- function(resp) {
    sse <- NULL
    while (is.null(sse) && !httr2::resp_stream_is_complete(resp)) {
        sse <- httr2::resp_stream_sse(resp)
        Sys.sleep(0.01)
    }
    sse
}

as_stub_closure <- function(x) {
    eval(parse(text = capture.output(args(x)))[[1L]])
}

function_formals <- function(x) {
    if (is.primitive(x))
        x <- as_stub_closure(x)
    formals(x)
}

strfit <- function(x, width = getOption("width")) {
    cli::ansi_strtrim(strwrap(x, width), width)
}

strwrap_preserve <- function(x, width = getOption("width")) {
    strwrap(strsplit(x, "\n", fixed = TRUE)[[1L]], width)
}

print.raster <- function(x, ...) {
    cat("<raster>: ", nrow(x), "x", ncol(x), "\n", sep = "")
}

print.nativeRaster <- function(x, ...) {
    cat("<nativeRaster>: ", nrow(x), "x", ncol(x), "\n", sep = "")
}

rename <- function(x, ...) {
    dots <- c(...)
    if (is.null(dots))
        stop("arguments in '...' must be named")
    names(x)[match(names(dots), names(x))] <- dots
    x
}

writable_props <- function(x) {
    stopifnot(inherits(x, S7_object))
    static_names <- names(Filter(Negate(prop_read_only),
                                 S7_class(x)@properties))
    props(x, static_names)
}

persist <- new_generic("persist", "x", function(x, file) S7_dispatch())
on_persist <- new_generic("on_persist", "x")

method(persist, class_any) <- function(x, file) {
    on_persist(x, file)
    saveRDS(x, file)
}

method(on_persist, class_any) <- function(x, ...) { }

method(on_persist, S7_object) <- function(x, ...) {
    for (p in writable_props(x)) {
        on_persist(p, ...)
    }
}

on_restore <- new_generic("on_restore", "x")

restore <- function(file) {
    obj <- readRDS(file)
    on_restore(obj, file)
}

method(on_restore, class_any) <- function(x, ...) x

method(on_restore, S7_object) <- function(x, ...) {
    p <- writable_props(x)
    props(x) <- lapply(p, on_restore, ...)
    x
}

R6_private <- function(x) {
    x$.__enclos_env__$private
}

rbind_list <- function(x) {
    all_columns <- unique(unlist(lapply(x, names)))

    do.call(rbind, lapply(x, function(df) {
        ensure_cols(df, all_columns)[all_columns]
    })) |> as.data.frame()
}

ensure_cols <- function(x, cols) {
    x[setdiff(cols, names(x))] <- list(rep(NA, nrow(x)))
    x
}

method(as.environment, S7_object) <- function(x) as.environment(props(x))

## Unused simple version that could go into base R
cumsum_breaks_simple <- function(x, threshold) {
    stopifnot(threshold >= 0L)
    cur_size <- 0L
    breaks <- integer()
    for (i in seq_along(x)) {
        cur_size <- cur_size + x[i]
        if (cur_size > threshold && i > 1L) {
            breaks <- c(breaks, i - 1L)
            cur_size <- x[i]
        }
    }
    c(breaks, length(x))
}

cumsum_breaks <- function(x, threshold, max_overlap = 0L) {
    stopifnot(threshold >= 0L, max_overlap <= threshold)
    cur_size <- 0L
    starts <- 1L
    ends <- integer()
    for (i in seq_along(x)) {
        cur_size <- cur_size + x[i]
        if (cur_size > threshold && i > 1L) {
            ends <- c(ends, i - 1L)
            max_overlap_i <- min(max_overlap, threshold - x[i])
            cur_overlap <- 0L
            j <- i - 1L
            while (j > 0L && cur_overlap + x[j] <= max_overlap_i) {
                cur_overlap <- cur_overlap + x[j]
                j <- j - 1L
            }
            starts <- c(starts, j + 1L)
            cur_size <- cur_overlap + x[i]
        }
    }
    ends <- c(ends, length(x))
    data.frame(starts, ends)
}

ScalarString := new_class(
    class_character,
    validator = \(self) {
        if (length(self) != 1L || is.na(self))
            "must be a single, non-NA string"
    })

File := new_class(ScalarString)
Text := new_class(ScalarString)
HubID := new_class(ScalarString)

resembles_file <- function(x) tools::file_ext(x) != ""

resembles_text <- function(x) !resembles_file(x)

resembles_hub_id <- function(x) length(strsplit(x, "/")[[1L]]) == 2L

resembles_url <- function(x, scheme) {
    x_scheme <- try(httr2::url_parse(x)$scheme, silent = TRUE)
    if (inherits(x_scheme, "try-error"))
        return(FALSE)
    if (!missing(scheme))
        identical(x_scheme, scheme)
    else TRUE
}

url_path_ends_with <- function(x, suffix) {
    path <- httr2::url_parse(x)$path
    endsWith(sub("/$", "", path), suffix)
}

read_as_string <- function(x) paste(readLines(x), collapse = "\n")

s3_super <- function(from, to) {
    m <- match(to$class[1L], class(from))
    if (is.na(m))
        stop("'from' is not an instance of '", to$class, "'")
    class(from) <- tail(class(from), 1L - m)
    from
}

named_list <- function(...) {
    ensure_named(list(...))
}

ensure_named <- function(x) {
    if (is.null(names(x)))
        names(x) <- as.character(seq_along(x))
    x
}

home_dir <- function() {
    if (.Platform$OS.type == "windows") {
        Sys.getenv("USERPROFILE")
    } else {
        path.expand("~")
    }
}

put <- function(x, `_list` = list(...), ...) {
    for (name in names(`_list`)) {
        x[[name]] <- `_list`[[name]]
    }
    x
}

download_file <- function(url, destfile, ...) {
    if (!dir.exists(dirname(destfile)))
        dir.create(dirname(destfile), recursive = TRUE)
    result <- curl::multi_download(url, destfile, resume = TRUE, ...)
    if (!result$success) {
        stop("Failed to download ", url, ": ", result$error)
    }
    invisible(TRUE)
}

prompt_download_file <- function(url, path, ...) {
    download <- !interactive() ||
        askYesNo(paste0("Do you want to download ", basename(url), "?"))
    invisible(download && download_file(url, path, ...))
}

file_cache_path <- function(url, type) {
    file.path(tools::R_user_dir("wizrd", which = "cache"), type,
              basename(url))
}

cache_file <- function(url, type) {
    path <- file_cache_path(url, type)
    if (file.exists(path) || prompt_download_file(url, path))
        path
    else stop("Failed to cache ", url)
}

verbose_message <- function(...) {
    if (getOption("wizrd_verbose", FALSE))
        message(...)
    invisible()
}

condition <- function(message, ..., class = NULL, call = NULL) {
    structure(list(message = as.character(message), call = call,
                   ...), class = c(class, "condition"))
}

emit <- function(cond) UseMethod("emit")

emit.default <- function(cond) signalCondition(cond)

emit.error <- function(cond) stop(cond)

emit.warning <- function(cond) warning(cond)

emit.message <- function(cond) message(cond)

emit.verbose_condition <- function(cond) verbose_message(cond)

is_port_open <- function(port) {
    con <- try(suppressWarnings(socketConnection(port = port, blocking = TRUE)),
               silent = TRUE)
    if (!inherits(con, "try-error")) {
        close(con)
        TRUE
    } else {
        FALSE
    }
}

find_available_port <- function(start = 8000, end = 8100) {
    stopifnot(start <= end)
    for (port in start:end) {
        if (!is_port_open(port)) {
            return(port)
        }
        Sys.sleep(0.01)
    }
    stop("No available ports found in ", start, ":", end)
}

wait_until_port_open <- function(port) {
    while (!is_port_open(port)) {
        Sys.sleep(0.01)
    }
    invisible(port)
}

RFC_5424_SEVERITY_LEVELS <- c("emergency", "alert", "critical", "error",
                              "warning", "notice", "info", "debug")

# API key cache management for httptest2
api_key_cache_dir <- function(base_dir, env_var) {
    api_key <- Sys.getenv(env_var)
    if (api_key == "") {
        return(base_dir)
    }
    key_hash <- substr(digest::digest(api_key, algo = "sha256"), 1, 8)
    file.path(base_dir, key_hash)
}

start_vignette_with_api_key <- function(base_dir, env_var) {
    httptest2::start_vignette(api_key_cache_dir(base_dir, env_var))
}
