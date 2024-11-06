assert_scalar <- function(scalar, class, arg = deparse(substitute(scalar)))
{
    if (length(scalar) != 1 || !S7:::class_inherits(scalar, class)) {
        type_name <- if (identical(class, class_numeric)) "numeric"
                     else class$class
        msg <- sprintf("`%s` must be a single %s value", arg, type_name)
        stop(msg, call. = FALSE)
    }
    if (is.na(scalar)) {
        msg <- sprintf("`%s` must not be NA", arg)
        stop(msg, call. = FALSE)
    }
}

nullable <- function(prop) {
    if (S7:::is_foundation_class(prop))
        prop <- new_property(prop)
    prop$class <- new_union(NULL, prop$class)
    prop["default"] <- list(NULL)
    prop
}

named <- function(prop) {
    if (S7:::is_foundation_class(prop))
        prop <- new_property(prop)
    validator <- prop$validator
    prop$validator <- \(value)
        c(if (is.null(names(value)))
            "must have names",
          if (!is.null(validator))
              validator(value))
    if (is.null(prop$default))
        prop$default <- S7:::prop_default(prop)
    if (is.null(names(prop$default)))
        names(prop$default) <- rep("", length(prop$default))
    prop
}

with_default <- function(prop, default) {
    prop$default <- default
    prop
}

new_scalar_property <- function(class, ..., validator = NULL, default) {
    assert_scalar(default, class)
    prop <- new_property(class, ..., validator = function(value) {
        if (is.null(value))
            return(NULL)
        c(if (length(value) != 1L || is.na(value))
            "must be of length one and not missing",
          if (!is.null(validator))
              validator(value)
          )
    }, default = default)
    class(prop) <- c("scalar_S7_property", class(prop))
    prop
}

new_string_property <- function(..., validator = NULL,
                                default = "", choices = NULL)
{
    prop <- new_scalar_property(
        class_character, ...,
        validator = function(value) {
            c(if (!is.null(choices) && !all(value %in% choices))
                paste("contains values not in",
                      deparse(choices)),
              if (!is.null(validator))
                  validator(value)
              )
        }, default = default
    )
    prop$choices <- choices
    class(prop) <- c("string_S7_property", class(prop))
    prop
}

## sensible pattern?

new_flag_property <- function(..., default = FALSE) {
    new_scalar_property(class_logical, ..., default = default)
}

## TODO: make this handle non-scalars as well
new_number_property <- function(class = class_numeric, ..., validator = NULL,
                                default = min(max(min, 0), max),
                                min = -Inf, max = Inf)
{
    prop <- new_scalar_property(class, ..., validator = function(value) {
        c(if (any(value < min))
              paste("must be >=", min),
          if (any(value > max))
              paste("must be <=", max),
          if (!is.null(validator))
              validator(value)
          )
    }, default = default)
    prop$min <- min
    prop$max <- max
    class(prop) <- c("numeric_S7_property", class(prop))
    prop
}

new_int_property <- function(..., default = min(max(min, 0L), max),
                             min = .Machine$integer.min,
                             max = .Machine$integer.max)
{
    new_number_property(class_integer, ..., default = default, min = min,
                        max = max)
}

prop_string <- new_string_property()
prop_flag <- new_flag_property()
prop_number <- new_number_property()
prop_number_nn <- new_number_property(min = 0)
prop_number_pos <- new_number_property(min = 1)
prop_prob <- new_number_property(min = 0, max = 1)
prop_int <- new_int_property()
prop_int_nn <- new_int_property(min = 0L)
prop_int_pos <- new_int_property(min = 1L)

new_list_property <- function(..., validator = NULL,
                              default = setNames(
                                  list(),
                                  if (isTRUE(named)) character()),
                              of = class_any, named = NA,
                              min_length = 0L, max_length = Inf)
{
    prop <- new_property(class_list, ..., validator = function(value) {
        c(if (!identical(of, class_any) &&
                  !all(vapply(value, S7:::class_inherits, logical(1L), of)))
            paste("must only contain elements of class", of@name),
          if (isTRUE(named) && is.null(names(value)))
              "must have names",
          if (identical(named, FALSE) && !is.null(names(value)))
              "must not have names",
          if (length(value) < min_length || length(value) > max_length)
              paste0("must have length in [", min_length, ", ", max_length, "]"),
          if (!is.null(validator))
              validator(value)
          )
    }, default = default)
    prop$of <- of
    prop$named <- named
    class(prop) <- c("list_S7_property", class(prop))
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
    } else EXPR[notfound]
    
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
    Rd_parse_args(Rd_for_tag("arguments"))
}

Rd_src <- function(Rd) {
    paste(as.character(attr(Rd, "srcref"), useSource = TRUE), collapse="\n")
}

Rd_parse_args <- function(args) {
    items <- Filter(\(x) attr(x, "Rd_tag") == "\\item", args)
    ans <- vapply(items, \(x) Rd_src(x[[2L]]), character(1L))
    ans_names <- vapply(items, \(x) gsub("\\dots", "...", Rd_src(x[[1L]])),
                        character(1L))
    ans_names_split <- strsplit(ans_names, ",", fixed = TRUE)
    setNames(rep(ans, lengths(ans_names_split)),
             trimws(unlist(ans_names_split)))
}

Rd_description <- function(Rd) {
    Rd_src(Rd_for_tag("description"))
}

Rd_value <- function(Rd) {
    Rd_src(Rd_for_tag("value"))
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

require_ns <- function(x, to) {
    assert_character(x)
    assert_string(to)

    loaded <- vapply(x, requireNamespace, logical(1L), quietly = TRUE)
    if (any(!loaded))
        stop("Install package", if (sum(loaded) > 1L) "s" else "", " ",
             paste0("'", x[!loaded], "'", collapse = " and "), " to ", to, ".",
             call. = FALSE)

    invisible(TRUE)
}

init_process <- function(path, args, ready_callback, error_callback,
                         poll_timeout = 10L)
{
    p <- processx::process$new(path, args, stdout = "|", stderr = "|")

    error <- ""
    output <- ""
    while(p$is_alive()) {
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
    assert_int(port, lower = 1024L, upper = 65535L)
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
    S7_class(x) %||% as_class(methods::getClassDef(class(x)[1L])) %||%
        new_S3_class(class(x))
}

strfit <- function(x, width = getOption("width")) {
    cli::ansi_strtrim(strwrap(x, width), width)
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
    static_names <- names(Filter(Negate(prop_read_only), S7_class(x)@properties))
    props(x, static_names)
}

persist <- new_generic("persist", "x")
on_persist <- new_generic("on_persist", "x")

method(persist, class_any) <- function(x, file) {
    on_persist(x, file)
    saveRDS(x, file)
}

method(on_persist, class_any) <- function(x, ...) { }

method(on_persist, S7_object) <- function(x, ...) {
    for (p in writable_props(x))
        on_persist(p, ...)
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
