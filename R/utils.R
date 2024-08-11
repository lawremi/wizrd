makeValidationFunction <- function(checker) {
    checker_call <- as.call(c(substitute(checker),
                              lapply(names(formals(checker)), as.name)))
    validate <- as.function(c(formals(checker), alist(.var.name = vname(x)),
                              substitute({
                                  ans <- checker_call
                                  if (!isTRUE(ans))
                                      paste(sQuote(.var.name), "is invalid:",
                                            ans)
                              })), envir = parent.frame())
    validate
}

validate_string <- makeValidationFunction(check_string)
validate_flag <- makeValidationFunction(check_flag)
validate_number <- makeValidationFunction(check_number)

new_scalar_property <- function(class, ..., validator = NULL, nullable = FALSE) {
    if (nullable) {
        class <- new_union(class, NULL)
    }
    prop <- new_property(class, ..., validator = function(prop) {
        c(if (nullable && is.null(prop))
            NULL
          else if (length(prop) != 1L || is.na(prop))
              "must be of length one and not missing",
          if (!is.null(validator))
              validator(prop)
          )
    })
    class(prop) <- c("scalar_S7_property", class(prop))
    prop
}

new_string_property <- function(..., validator = NULL, choices = NULL) {
    prop <- new_scalar_property(class_character,
                        validator = function(prop) {
                            c(if (!is.null(choices) && !all(prop %in% choices))
                                paste("contains values not in",
                                      deparse(choices)),
                              if (!is.null(validator))
                                  validator(prop)
                              )
                        })
    prop$choices <- choices
    class(prop) <- c("string_S7_property", class(prop))
    prop
}

## sensible pattern?
prop_string <- new_string_property()
prop_string_nullable <- new_string_property(nullable = TRUE)

new_flag_property <- function(...) {
    new_scalar_property(class_logical, ...)
}

prop_flag <- new_flag_property()

prop_number <- new_number_property()

## TODO: make this handle non-scalars as well
new_number_property <- function(class = class_numeric, ..., validator = NULL,
                                min = -Inf, max = Inf)
{
    prop <- new_scalar_property(class, ..., validator = function(prop) {
        c(if (any(prop < min))
              paste("must be >=", min),
          if (any(prop > max))
              paste("must be <=", max),
          if (!is.null(valdiator))
              validator(prop)
          )
    })
    prop$min <- min
    prop$max <- max
    class(prop) <- c("numeric_S7_property", class(prop))
    prop
}

prop_int <- new_int_property()
prop_int_nn <- new_int_property(low = 0L)
prop_int_pos <- new_int_property(low = 1L)

new_int_property <- function(...)
{
    new_number_property(class_integer, ...)
}

new_list_property <- function(..., validator = NULL, of = class_any,
                              named = FALSE)
{
    prop <- new_property(class_list, ..., validator = function(prop) {
        c(if (!identical(of, class_any) &&
                  !all(vapply(prop, inherits, logical(1L), of)))
            paste("must only contain elements of class", of$name),
          if (named && is.null(names(prop)))
              "must have names",
          if (!is.null(validator))
              valdiator(prop)
          )
    })
    prop$of <- of
    prop$named <- named
    class(prop) <- c("list_S7_property", class(prop))
    prop
}

nullable <- function(class) {
    new_union(NULL, class)
}

new_as_generic <- function(class) {
    stopifnot(inherits(class, "S7_class"))
    generic <- new_generic(paste0("as_", class@name), "from")
    method(generic, class) <- identity
    generic
}

make_command <- function(command, ...) {
    args <- list(...)
    names(args) <- sub("_", "-", names(args), fixed = TRUE)
    logicals <- vapply(args, is.logical, logical(1L))
    args[logicals] <- lapply(args[logicals], function(x) if (x) "")
    argstr <- paste0("--", names(args), " ", args, collapse = " ")
    paste(command, argstr)
}

get_api_key <- function(prefix) {
    key_name <- paste0(prefix, "_API_KEY")
    key <- Sys.getenv(key_name)
    if (key == "")
        stop("$", key_name, " is not set")
    key
}

image_data_uri <- function(x) {
    if (!requireNamespace("base64enc") || !requireNamespace("png"))
        stop("Please install 'base64enc' and 'png' packages to encode images")
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

find_name <- function(what, env) {
    objs <- as.list(env)
    pos <- Position(identical, objs, what)
    if (!is.na(pos))
        names(objs)[pos]
}
