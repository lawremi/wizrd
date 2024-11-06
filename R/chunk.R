Chunking := new_class(abstract = TRUE)

ElementChunking := new_class(
    Chunking,
    properties = list(
        size = prop_number_pos,
        overlap = prop_number_nn
    ),
    validator = \(self) {
        if (self@size < self@overlap)
            "@size must be greater than @overlap"
    })

CharacterChunking := new_class(ElementChunking)

WordChunking := new_class(ElementChunking)

ParagraphChunking := new_class(ElementChunking)

HierarchicalChunking := new_class(
    Chunking,
    properties = list(
        section_chunking = NULL | Chunking
    )
)

MarkdownChunking := new_class(HierarchicalChunking)
    
RMarkdownChunking := new_class(MarkdownChunking)

QuartoChunking := new_class(MarkdownChunking)

HTMLChunking := new_class(HierarchicalChunking)

PDFChunking := new_class(HierarchicalChunking)

RdChunking := new_class(Chunking)

chunk := new_generic(c("x", "by"))

method(chunk, list(class_any, NULL)) <- function(x, by) x

ext_to_chunking <- local({
    map <- list()
    function(...) {
        args <- list(...)
        if (length(args) == 0L)
            map
        else if (!is.null(names(args))) {
            old_map <- map
            map[names(args)] <<- args
            old_map
        } else if (length(args) == 1L)
            map[args[[1L]]]
        else stop("multiple arguments must be named")
    }
})

ext_to_chunking(Rmd = RMarkdownChunking(), Qmd = QuartoChunking(),
                Rd = RdChunking(), html = HTMLChunking(), pdf = PDFChunking())

default_chunking := new_generic("x")

method(default_chunking, class_character) <- function(x) {
    ext_to_chunking()
}

method(chunk, list(class_any, class_missing)) <- function(x, by) {
    chunk(x, chunking(x))
}

is_text <- function(x)
    if (is.character(x)) grepl("\n", x) else rep(TRUE, length(x))

chunk_text_or_file <- function(x, by) {
    stopifnot(length(x) == 1L && !is.na(x))
    is_file <- !is_text(x)
    if (is_file) {
        if (file.info(x)[,"isdir"])
            chunk(list.files(x, full.names = TRUE), by)
        else chunk_file(x, by)
    } else chunk_text(x, by)
}

method(chunk, list(class_character | class_list, Chunking | class_list)) <-
    function(x, by) {
        by <- if (is.list(by)) {
            path <- ifelse(is_text(x), names(x) %||% list(NULL), x)
            by[match(tolower(tools::file_ext(path)), tolower(names(by)))]
        } else list(by)
        chunks <- Map(chunk_text_or_file, x, by)
        source <- names(x) %||% ifelse(is_text(x), seq_along(x), x)
        data.frame(source = rep(source, sapply(chunks, nrow)),
                   text = do.call(rbind, chunks))
    }

chunk_file := new_generic(c("x", "by"))

method(chunk_file, list(class_character, class_any)) <- function(x, by) {
    chunk_text(readLines(x) |> paste(collapse = "\n"), by)
}

chunk_text := new_generic(c("x", "by"))

method(chunk_text, list(class_character, class_any)) <- function(x, by) {
    stopifnot(length(x) == 1L && !is.na(x))
    path <- tempfile()
    writeLines(x, path)
    chunk_file(path, by)
}

chunk_starts <- function(by, len) {
    starts <- seq(1L, len, by = by@size - by@overlap)
    starts[len - starts >= by@overlap]
}

method(chunk_text, list(class_character, CharacterChunking)) <- function(x, by)
{
    stopifnot(length(x) == 1L && !is.na(x))
    starts <- chunk_starts(by, nchar(x))
    text <- vapply(starts, \(s) substring(x, s, s + by@size - 1L), character(1L))
    data.frame(text)
}

chunk_elements <- function(x, by, pattern) {
    stopifnot(length(x) == 1L && !is.na(x))
    words <- strsplit(x, pattern)[[1L]]
    starts <- chunk_starts(length(words), by)
    text <- vapply(starts, \(s) {
        paste(words[seq(s, s + by@size - 1L)], collapse = " ")
    }, character(1L))
    data.frame(text)
}

method(chunk_text, list(class_character, WordChunking)) <- function(x, by)
{
    chunk_elements(x, by, "\\s+")
}

method(chunk_text, list(class_character, ParagraphChunking)) <- function(x, by)
{
    chunk_elements(x, by, "\n(\\s*\n)+")
}

method(chunk_text, list(class_character, RMarkdownChunking)) <- function(x, by)
{
    stopifnot(length(x) == 1L && !is.na(x))
    require_ns("parsermd", "chunk RMarkdown")
    chunk(parsermd::parse_rmd(x), by)
}

method(chunk_text, list(class_character, QuartoChunking)) <- function(x, by)
{
    stopifnot(length(x) == 1L && !is.na(x))
    require_ns("parsermd", "chunk Quarto")
    chunk(parsermd::parse_qmd(x), by)
}

rmd_ast <- new_S3_class("rmd_ast")
rmd_chunk <- new_S3_class("rmd_chunk")
rmd_markdown <- new_S3_class("rmd_markdown")

method(chunk, list(rmd_chunk | rmd_markdown, Chunking)) <- function(x, by) {
    data.frame(text = chunk(parsermd::as_document(x), by))
}

method(chunking, rmd_ast) <- function(x) MarkdownChunking()

method(chunk, list(rmd_ast, MarkdownChunking)) <- function(x, by) {
    df <- as.data.frame(x)
    leaf_types <- c("rmd_chunk", "rmd_markdown")
    df_leaves <- subset(df, type %in% leaf_types)
    chunks <- lapply(df_leaves$ast, chunk, by@section_chunking)
    meta_cols <- c(intersect(names(df_leaves), paste0("sec_h", 1:6)), "label")
    metadata <- df_leaves[rep(seq_len(nrow(df_leaves)), sapply(chunks, nrow)),
                          meta_cols]
    metadata$title <- unlist(subset(df, type == "rmd_yaml_list")$ast,
                             recursive = FALSE)$title
    data.frame(metadata, text = do.call(rbind, chunks))
}

method(chunk_file, list(class_character, HTMLChunking)) <- function(x, by) {
    stopifnot(length(x) == 1L && !is.na(x))
    require_ns("rmarkdown", "chunk HTML")
    output <- tempfile(fileext = "md")
    rmarkdown::pandoc_convert(x, "md", "html", output)
    chunk_file(output, by@section_chunking)
}

method(chunk_file, list(class_character, PDFChunking)) <- function(x, by) {
    stopifnot(length(x) == 1L && !is.na(x))
    require_ns("pdftools", "chunk PDF")
    chunk_text(paste(pdftools::pdf_text(x), collapse = "\n"),
               by@section_chunking)
}

packageIQR <- new_S3_class("packageIQR")

method(default_chunking, packageIQR) <- function(x) ext_to_chunking()

method(chunk, list(packageIQR, class_list)) <- function(x, by) {
    if (x$type != "vignette")
        stop("Only 'packageIQR' objects of type 'vignette' are supported")
    assert_named(by)
    
    r <- x$results
    path <- file.path(r[,"LibPath"], r[,"Package"], "doc", r[,"Item"]) |>
        outer(names(by), \(x, y) paste0(x, ".", y))
    chunk(path[file.exists(path)], by)
}

Rd <- new_S3_class("Rd")

Rd_sections <- c("description", "usage", "arguments", "details", "section",
                 "value", "examples")

method(default_chunking, Rd) <- function(x) RdChunking()

method(chunk, list(Rd, RdChunking)) <- function(x, by) {
    names(x) <- gsub("\\", "", vapply(x, attr, character(1L), "Rd_tag"),
                     fixed = TRUE)
    text <- c(file = Rd_src(x),
              vapply(x[names(x) %in% Rd_sections], Rd_src, character(1L)))
    data.frame(text = unlist(chunks, use.names = FALSE),
               aliases = paste(Rd_aliases(f), collapse = ","),
               section = names(text))
}

Rd_aliases <- function(x) {
    unlist(Filter(\(xi) attr(xi, "Rd_tag") == "\\alias", x), use.names = FALSE)
}
