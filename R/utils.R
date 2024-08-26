new_scalar_property <- function(class, ..., validator = NULL, nullable = FALSE) {
    if (nullable) {
        class <- new_union(class, NULL)
    }
    prop <- new_property(class, ..., validator = function(value) {
        c(if (nullable && is.null(value))
            NULL
          else if (length(value) != 1L || is.na(value))
              "must be of length one and not missing",
          if (!is.null(validator))
              validator(value)
          )
    })
    class(prop) <- c("scalar_S7_property", class(prop))
    prop
}

new_string_property <- function(..., validator = NULL,
                                default = if (!nullable) "",
                                nullable = FALSE, choices = NULL)
{
    assert_flag(nullable)
    prop <- new_scalar_property(
        class_character,
        validator = function(value) {
            c(if (!is.null(choices) && !all(value %in% choices))
                paste("contains values not in",
                      deparse(choices)),
              if (!is.null(validator))
                  validator(value)
              )
        }, default = default, nullable = nullable)
    prop$choices <- choices
    class(prop) <- c("string_S7_property", class(prop))
    prop
}

## sensible pattern?
prop_string <- new_string_property()
prop_string_nullable <- new_string_property(nullable = TRUE)

new_flag_property <- function(..., nullable = FALSE,
                              default = if (!nullable) FALSE) {
    assert_flag(nullable)
    new_scalar_property(class_logical, ..., default = default,
                        nullable = nullable)
}

prop_flag <- new_flag_property()

## TODO: make this handle non-scalars as well
new_number_property <- function(class = class_numeric, ..., validator = NULL,
                                nullable = FALSE,
                                default = if (!nullable) min(max(min, 0L), max),
                                min = -Inf, max = Inf)
{
    assert_flag(nullable)
    prop <- new_scalar_property(class, ..., validator = function(value) {
        c(if (any(value < min))
              paste("must be >=", min),
          if (any(value > max))
              paste("must be <=", max),
          if (!is.null(valdiator))
              validator(value)
          )
    }, nullable = nullable)
    prop$min <- min
    prop$max <- max
    class(prop) <- c("numeric_S7_property", class(prop))
    prop
}

prop_number <- new_number_property()

new_int_property <- function(..., min = .Machine$integer.min,
                             max = .Machine$integer.max)
{
    new_number_property(class_integer, ..., min = min, max = max)
}

prop_int <- new_int_property()
prop_int_nn <- new_int_property(min = 0L)
prop_int_pos <- new_int_property(min = 1L)

new_list_property <- function(..., validator = NULL, of = class_any, named = NA)
{
    prop <- new_property(class_list, ..., validator = function(value) {
        c(if (!identical(of, class_any) &&
                  !all(vapply(value, inherits, logical(1L), of)))
            paste("must only contain elements of class", of@name),
          if (isTRUE(named) && is.null(names(value)))
              "must have names",
          if (identical(named, FALSE) && !is.null(names(value)))
              "must not have names",
          if (!is.null(validator))
              valdiator(value)
          )
    })
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
    if (identical(names(cases)[length(cases)], ""))
        ans[is.na(names(ans)) & !is.na(EXPR)] <- cases[[length(cases)]]

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

image_data_uri <- function(x) {
    require_ns(c("base64enc", "png"), "encode images")
    
    if (inherits(x, "nativeRaster")) { # like from dev.capture() or readPNG()
        image <- x
    } else {
        m <- as.matrix(as.raster(x))
        rgb <- col2rgb(m, alpha = TRUE) / 255L
        image <- array(t(rgb), c(dim(m), 4L))
    }
    
    base64enc::dataURI(png::writePNG(image), mime = "image/png")
}

get_Rd <- function(topic, package = NULL) {
    files <- help(topic, identity(package))
    if (length(files) > 1L)
        stop("Multiple help files found for ", topic, ". Specify the package.")
    if (length(files) == 0L)
        return(NULL)
    filename <- paste0(basename(files), ".Rd")
    package <- basename(dirname(dirname(files)))
    Rd <- tools::Rd_db(package)[[filename]]
    text <- capture.output(print(Rd))
    paste(text, collapse = "\n")
}

Rd_args <- function(Rd) {
    args <- Find(function(x) attr(x, "Rd_tag") == "\\arguments", Rd)
    items <- Filter(function(x) attr(x, "Rd_tag") == "\\item", args)
    ans <- lapply(items, function(x) paste(unlist(x[[2L]]), collapse = ""))
    ans_names <- lapply(items, function(x) unlist(x[[1L]]))
    ans_names_split <- strsplit(ans_names, ",", fixed = TRUE)
    setNames(rep(ans, lengths(ans_names_split)),
             trimws(unlist(ans_names_split)))
}

Rd_description <- function(Rd) {
    desc <- Find(function(x) attr(x, "Rd_tag") == "\\description", Rd)
    paste(unlist(desc), collapse = "")
}

Rd_value <- function(Rd) {
    desc <- Find(function(x) attr(x, "Rd_tag") == "\\value", Rd)
    paste(unlist(desc), collapse = "")
}

Rd_for_function <- function(FUN, name = deparse(substitute(FUN))) {
    if (isNamespace(environment(FUN))) {
        package <- getNamespaceName(environment(fun))
        Rd <- tryCatch(get_Rd(name, package), error = NULL)
        if (is.null(Rd)) {
            name <- find_name(FUN, environment(FUN))
            if (!is.null(name))
                get_Rd(name, package)
        }
    }
}

find_name <- function(what, env) {
    objs <- as.list(env)
    pos <- Position(identical, objs, what)
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
             paste0("'", x[!loaded], "'", collapse = " and "), " to ", to, ".")

    invisible(TRUE)
}

init_process <- function(path, args, ready_callback, error_callback,
                         timeout = 10L)
{
    p <- processx::process$new(path, args, stdout = "|", stderr = "|")

    error <- ""
    output <- ""
    while(p$is_alive()) {
        io <- p$poll_io(timeout)
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

    invisible(TRUE)
}

assert_port <- function(port) {
    assert_int(port, lower = 1024L, upper = 65535L)
}
