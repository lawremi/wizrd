assert_scalar <- function(scalar, class, arg = deparse(substitute(scalar))) {
    if (length(scalar) != 1L || !S7:::class_inherits(scalar, class)) {
        type_name <- if (identical(class, class_numeric)) {
            "numeric"
        } else if (inherits(class, "S7_union")) {
            paste(class$classes, collapse = " | ")
        } else {
            class$class
        }
        msg <- sprintf("`%s` must be a single %s value", arg, type_name)
        stop(msg, call. = FALSE)
    }
    if (is.na(scalar)) {
        msg <- sprintf("`%s` must not be NA", arg)
        stop(msg, call. = FALSE)
    }
}

nullable <- function(prop) {
    if (is.null(prop) || S7:::is_foundation_class(prop))
        prop <- new_property(prop)
    prop$class <- new_union(NULL, prop$class)
    if (identical(prop$default, missing_name()))
        prop["default"] <- list(NULL)
    prop
}

optional <- function(prop) {
    prop <- nullable(prop)
    class(prop) <- c("optional_S7_property", class(prop))
    prop
}

named <- function(prop) {
    if (S7:::is_foundation_class(prop))
        prop <- new_property(prop)
    validator <- prop$validator
    prop$validator <- \(value) {
        c(
            if (is.vector(value) && is.null(names(value)))
                "must have names",
            if (!is.null(validator))
                validator(value)
        )
    }
    if (is.null(prop$default))
        prop$default <- S7:::prop_default(prop)
    if (!is.null(prop$default) && is.null(names(prop$default)))
        names(prop$default) <- rep("", length(prop$default))
    prop
}

with_default <- function(prop, default) {
    prop$default <- default
    prop
}

literal <- function(value) {
    prop <- new_property(class_object(value),
                         validator = eval(substitute(function(value) {
                             if (!identical(value, VALUE))
                                 paste("must match", VALUE)
                         }, list(VALUE = value))),
                         default = value)
    if (is.vector(value) && length(value) == 1L)
        prop <- scalar(prop)
    class(prop) <- c("literal_S7_property", class(prop))
    prop$value <- value
    prop
}

static_inherits <- function(x, parent) {
    while (!is.null(x) && !identical(x@parent, parent))
        x <- x@parent
    !is.null(x)
}

S7_class_with_parent <- function(parent, validator = NULL, ...) {
    new_property(S7_class, validator = function(value) {
        c(if (!static_inherits(value, parent))
              paste("must be a descendant of", parent@name),
          if (!is.null(validator))
              validator(value))
    }, ...)
}

missing_name <- function() alist(x = )[[1L]]

scalar <- function(x, ..., validator = x$validator, default = x$default,
                   choices = NULL, min = -Inf, max = Inf) {
    if (S7:::is_foundation_class(x))
        x <- new_property(x, ...)
    stopifnot(inherits(x, "S7_property"))
    x$default <- if (is.null(default)) {
        if (inherits(x$class, "S7_union") && is.null(x$class$classes[[1L]]))
            NULL
        else if (length(choices) > 0L)
            choices[1L]
        else missing_name()
    } else {
        if (!is.language(default))
            assert_scalar(default, x$class)
        default
    }
    force(validator)
    x$validator <- function(value) {
        if (is.null(value))
            return(NULL)
        c(
            if (length(value) != 1L || is.na(value))
                "must be of length one and not missing",
            if (!is.null(choices) && !all(value %in% choices))
                paste("contains values not in", deparse(choices)),
            if (is.numeric(value))
                c(
                    if (any(value < min))
                        paste("must be >=", min),
                    if (any(value > max))
                        paste("must be <=", max)
                ),
            if (!is.null(validator))
                validator(value)
        )
    }
    class(x) <- c("scalar_S7_property", class(x))
    x
}

list_of <- function(class, ...) {
    new_list_property(of = class, ...)
}

new_list_property <- function(..., validator = NULL,
                              default = if (isTRUE(named))
                                  quote(setNames(list(), character()))
                              else quote(list()),
                              of = class_any, named = NA,
                              min_length = 0L, max_length = Inf) {
    prop <- new_property(class_list, ..., validator = function(value) {
        c(
            if (!identical(of, class_any) &&
                    !all(vapply(value, S7:::class_inherits, logical(1L), of)))
                paste("must only contain elements of class",
                      S7:::class_desc(of)),
            if (!is.null(of_validator)) {
                msgs <- unlist(lapply(value, of_validator))
                if (length(msgs) > 0L) {
                    paste("element(s) failed validation:",
                          paste0("'", unique(msgs), "'", collapse = ", "))
                }
            },
            if (isTRUE(named) && is.null(names(value)))
                "must have names",
            if (identical(named, FALSE) && !is.null(names(value)))
                "must not have names",
            if (length(value) < min_length || length(value) > max_length)
                paste0("must have length in [", min_length, ", ", max_length,
                       "]"),
            if (!is.null(validator))
                validator(value)
        )
    }, default = default)
    prop$of <- of
    if (inherits(of, "S7_property")) {
        of_validator <- of$validator
        of <- of$class
    } else {
        of_validator <- NULL
    }
    prop$named <- named
    class(prop) <- c("list_S7_property", class(prop))
    prop
}

zero_row_data_frame <- function(col.names) {
    data.frame(matrix(nrow = 0L, ncol = length(col.names))) |>
        setNames(col.names)
}

new_data_frame_property <- function(..., validator = NULL,
                                    col.names = colnames(prototype),
                                    default = substitute(prototype) %||%
                                        zero_row_data_frame(col.names),
                                    prototype = NULL) {
    types <- lapply(prototype, class_object)
    prop <- new_property(class_data.frame, ..., validator = function(value) {
        c(
            if (!is.null(col.names) &&
                    !identical(colnames(value), col.names))
                paste("colnames() must be", deparse(col.names))
            else if (!is.null(prototype)) {
                wrong_type <- !mapply(inherits, value, types)
                if (any(wrong_type))
                    paste(colnames(value)[wrong_type], "must be a",
                          vapply(types[wrong_type], S7:::class_desc,
                                 character(1L)),
                          collapse = ", ")
            },
            if (!is.null(validator))
                validator(value)
        )
    }, default = default)
    prop$prototype <- prototype
    prop$col.names <- col.names
    class(prop) <- c("data_frame_S7_property", class(prop))
    prop
}

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
    cat(S7:::obj_desc(x), ""); print(x@process)
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

## Destined for S7?

method(convert, list(class_any, class_logical)) <-
    function(from, to, ...) as.logical(from, ...)
method(convert, list(class_any, class_integer)) <-
    function(from, to, ...) as.integer(from, ...)
method(convert, list(class_any, class_double)) <-
    function(from, to, ...) as.double(from, ...)
method(convert, list(class_any, class_complex)) <-
    function(from, to, ...) as.complex(from, ...)
method(convert, list(class_any, class_character)) <-
    function(from, to, ...) as.character(from, ...)
method(convert, list(class_any, class_raw)) <-
    function(from, to, ...) as.raw(from, ...)
method(convert, list(class_any, class_list)) <-
    function(from, to, ...) as.list(from, ...)
method(convert, list(class_any, class_expression)) <-
    function(from, to, ...) as.expression(from, ...)
method(convert, list(class_any, class_function)) <-
    function(from, to, ...) as.function(from, ...)
method(convert, list(class_any, class_environment)) <-
    function(from, to, ...) as.environment(from, ...)
method(convert, list(class_any, class_name)) <-
    function(from, to, ...) as.name(from, ...)
method(convert, list(class_any, class_call)) <-
    function(from, to, ...) as.call(from, ...)

method(convert, list(class_any, class_data.frame)) <-
    function(from, to, ...) as.data.frame(from, ...)
method(convert, list(class_any, class_Date)) <-
    function(from, to, ...) as.Date(from, ...)
method(convert, list(class_any, class_factor)) <-
    function(from, to, ...) as.factor(from, ...)
method(convert, list(class_any, class_POSIXct)) <-
    function(from, to, ...) as.POSIXct(from, ...)
method(convert, list(class_any, class_POSIXlt)) <-
    function(from, to, ...) as.POSIXlt(from, ...)
method(convert, list(class_any, class_formula)) <-
    function(from, to, ...) as.formula(from, ...)

S7_class <- new_S3_class("S7_class")
S7_any <- new_S3_class("S7_any", constructor = \(.data) class_any)
S7_base_class <- new_S3_class("S7_base_class")
S7_union <- new_S3_class("S7_union")
S7_S3_class <- new_S3_class("S7_S3_class")

union_classes <- S7_any | NULL | S7_class | S7_base_class | S7_union |
    S7_S3_class | getClass("classRepresentation")

S7_property <- new_S3_class("S7_property")
scalar_S7_property <- new_S3_class(c("scalar_S7_property", "S7_property"))
string_S7_property <- new_S3_class(c("string_S7_property", "S7_property"))
list_S7_property <- new_S3_class(c("list_S7_property", "S7_property"))
numeric_S7_property <- new_S3_class(c("numeric_S7_property", "S7_property"))
data_frame_S7_property <- new_S3_class(c("data_frame_S7_property",
                                         "S7_property"))
optional_S7_property <- new_S3_class(c("optional_S7_property", "S7_property"))

class_json <- new_S3_class("json") # from jsonlite

nameOfClass.S7_S3_class <- function(x) x$class[1L]

as_stub_closure <- function(x) {
    eval(parse(text = capture.output(args(x)))[[1L]])
}

function_formals <- function(x) {
    if (is.primitive(x))
        x <- as_stub_closure(x)
    formals(x)
}

class_object <- function(x) {
    if (is.null(x))
        return(NULL)
    S7_class(x) %||% as_class(getClassDef(class(x)[1L])) %||%
        new_S3_class(class(x))
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

prop_read_only <- function(prop) {
    !is.null(prop$getter) && is.null(prop$setter)
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

## To S7?
method(convert, list(S7_class, S7_property)) <- function(from, to, ...) {
    new_property(from, ...)
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

RFC_5424_SEVERITY_LEVELS <- c("emergency", "alert", "critical", "error", "warning",
                              "notice", "info", "debug")
