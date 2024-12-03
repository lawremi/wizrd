Chunking := new_class(abstract = TRUE)

TokenChunking := new_class(
    Chunking,
    properties = list(
        token_limit = with_default(prop_number_pos, 512L),
        max_overlap = with_default(prop_number_nn, 64L)
    ),
    validator = \(self) {
        if (self@token_limit < self@max_overlap)
            "@token_limit must be greater than @max_overlap"
    })

## Algorithms for text chunking:

## Find token breaks and align to semantic boundaries, like sentences
## or paragraphs. Restrict to a given token count, and allow for a
## specified overlap with the previous chunk.

SentenceAlignedTokenChunking := new_class(TokenChunking)

HierarchicalChunking := new_class(
    Chunking,
    properties = list(
        section_chunking = new_property(
            NULL | Chunking,
            default = quote(SentenceAlignedTokenChunking())
        )
    )
)

MarkdownChunking := new_class(HierarchicalChunking)

RMarkdownChunking := new_class(MarkdownChunking)

QuartoChunking := new_class(MarkdownChunking)

HTMLChunking := new_class(HierarchicalChunking)

PDFChunking := new_class(HierarchicalChunking)

RdChunking := new_class(Chunking)

ScalarString := new_class(
    class_character,
    validator = \(self) {
        if (length(self) != 1L || is.na(self))
            "must be a single, non-NA string"
    })

File := new_class(ScalarString)
Text := new_class(ScalarString)

method(as.data.frame, Text) <-
    function(x, row.names = NULL, optional = FALSE, ...) {
        as.data.frame(S7_data(x), row.names = row.names,
                      optional = optional, ...)
    }

chunk := new_generic(c("x", "by"))

method(chunk, list(Text | class_character, NULL)) <- function(x, by)
    data.frame(text = x)

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

ext_to_chunking(SentenceAlignedTokenChunking(), md = MarkdownChunking(),
                Rmd = RMarkdownChunking(), Qmd = QuartoChunking(),
                Rd = RdChunking(), html = HTMLChunking(), pdf = PDFChunking())

default_chunking := new_generic("x")

method(default_chunking, class_character | class_list) <- function(x) {
    ext_to_chunking()
}

method(chunk, list(class_any, class_missing)) <- function(x, by, ...) {
    chunk(x, default_chunking(x), ...)
}

is_text <- function(x)
    if (is.character(x)) tools::file_ext(x) == "" else rep(TRUE, length(x))

chunk_text_or_file <- function(x, by, ...) {
    props(by) <- list(...)
    is_file <- is.character(x) && !is_text(x)
    if (is_file) {
        if (file.info(x)[,"isdir"])
            chunk(list.files(x, full.names = TRUE), by)
        else chunk(File(x), by)
    } else chunk(if(is.character(x)) Text(x) else x, by)
}

method(chunk, list(class_character | class_list, Chunking | class_list)) <-
    function(x, by, ...) {
        by <- if (is.list(by)) {
            path <- ifelse(is_text(x), names(x) %||% list(NULL), x)
            by[match(tolower(tools::file_ext(path)), tolower(names(by)))]
        } else list(by)
        chunks <- Map(chunk_text_or_file, x, by, MoreArgs = list(...))
        source <- names(x) %||% ifelse(is_text(x), seq_along(x), x)
        data.frame(source = rep(source, sapply(chunks, nrow)),
                   rbind_list(unname(chunks)) |> ensure_cols("text"))
    }

method(chunk, list(File, class_any)) <- function(x, by) {
    chunk(Text(readLines(x) |> paste(collapse = "\n")), by)
}

method(chunk, list(Text, class_any)) <- function(x, by) {
    file <- tempfile()
    writeLines(x, file)
    chunk(File(file), by)
}

chunk_starts <- function(by, len) {
    starts <- seq(1L, len, by = by@token_limit - by@max_overlap)
    starts[len - starts >= by@max_overlap]
}

method(chunk, list(Text, TokenChunking)) <- function(x, by)
{
    stopifnot(length(x) == 1L && !is.na(x))

    token_boundaries <- stringi::stri_locate_all_boundaries(text, type = "word")
    starts <- chunk_starts(nrow(token_boundaries), by)
    ends <- c(starts[-1L] - 1L, length(starts))
    text <- substring(x, token_boundaries[starts, "start"],
                      token_boundaries[ends, "end"])
    
    data.frame(text)
}

method(chunk, list(Text, SentenceAlignedTokenChunking)) <- function(x, by)
{
    require_ns("stringi", "generate sentence-aligned chunks")
    sentence_boundaries <-
        stringi::stri_locate_all_boundaries(text, type = "sentence")[[1L]]
    token_boundaries <- stringi::stri_locate_all_words(text)[[1L]]

    sentence_starts <- sentence_boundaries[, "start"]
    sentence_ends <- sentence_boundaries[, "end"]
    token_starts <- token_boundaries[, "start"]
    token_indices <- findInterval(token_starts, sentence_starts)
    token_counts <- tabulate(token_indices, nbins = length(sentence_starts))

    chunk_indices <- cumsum_breaks(token_counts, by@token_limit, by@max_overlap) 
    chunk_starts <- sentence_starts[chunk_indices$starts]
    chunk_ends <- sentence_ends[chunk_indices$ends]
    text <- substring(text, chunk_starts, chunk_ends)
    
    data.frame(text)
}

method(chunk, list(Text, RMarkdownChunking)) <- function(x, by)
{
    require_ns("parsermd", "chunk RMarkdown")
    chunk(parsermd::parse_rmd(x), by)
}

method(chunk, list(Text, QuartoChunking)) <- function(x, by)
{
    stopifnot(length(x) == 1L && !is.na(x))
    require_ns("parsermd", "chunk Quarto")
    chunk(parsermd::parse_qmd(x), by)
}

rmd_ast <- new_S3_class("rmd_ast")
rmd_chunk <- new_S3_class("rmd_chunk")
rmd_markdown <- new_S3_class("rmd_markdown")

chunk_rmd <- function(x, by) {
    parsermd::as_document(x) |> paste(collapse = "\n") |> Text() |> chunk(by)
}

method(chunk, list(rmd_chunk | rmd_markdown, class_any)) <- function(x, by) {
    chunk_rmd(x, by)
}

method(default_chunking, rmd_ast) <- function(x) MarkdownChunking()

method(chunk, list(rmd_ast, MarkdownChunking)) <- function(x, by) {
    df <- as.data.frame(x)
    leaf_types <- c("rmd_chunk", "rmd_markdown")
    df_leaves <- subset(df, type %in% leaf_types)
    sec_cols <- paste0("sec_h", 1:6)
    grp_cols <- intersect(names(df_leaves), sec_cols)
    chunks <- aggregate(df_leaves["ast"], df_leaves[grp_cols],
                        \(ast) chunk_rmd(ast, by@section_chunking),
                        simplify = FALSE)
    expand_ind <- rep(seq_len(nrow(chunks)), sapply(chunks$ast, nrow))
    meta <- chunks[expand_ind, grp_cols, drop = FALSE] |>
        ensure_cols(sec_cols) |> _[sec_cols]
    title <- unlist(subset(df, type == "rmd_yaml_list")$ast,
                    recursive = FALSE)$title
    cbind(title = rep(title, nrow(meta)), meta, do.call(rbind, chunks$ast))
}

method(chunk, list(File, HTMLChunking)) <- function(x, by) {
    require_ns("rmarkdown", "chunk HTML")
    output <- tempfile(fileext = "md")
    rmarkdown::pandoc_convert(x, "md", "html", output)
    chunk(output, by@section_chunking)
}

method(chunk, list(File, PDFChunking)) <- function(x, by) {
    require_ns("pdftools", "chunk PDF")
    chunk(paste(pdftools::pdf_text(x), collapse = "\n"), by@section_chunking)
}

packageIQR <- new_S3_class("packageIQR")

method(default_chunking, packageIQR) <- function(x) ext_to_chunking()

method(chunk, list(packageIQR, class_list)) <- function(x, by) {
    if (x$type != "vignette")
        stop("Only 'packageIQR' objects of type 'vignette' are supported")
    assert_named(by)
    r <- x$results
    if (nrow(r) == 0L)
        return(chunk(character(), by))
    path <- file.path(r[,"LibPath"], r[,"Package"], "doc", r[,"Item"]) |>
        outer(names(by), \(x, y) paste0(x, ".", y))
    path_existant <- path[file.exists(path)]
    path_existant <-
        path_existant[!duplicated(tools::file_path_sans_ext(path_existant))]
    chunk(path_existant, by)
}

Rd <- new_S3_class("Rd")

method(default_chunking, Rd) <- function(x) RdChunking()

method(chunk, list(Rd, RdChunking)) <- function(x, by) {
    data.frame(text = Rd_src(x))
}
