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

MarkdownChunking := new_class(
    Chunking,
    properties = list(
        ## Each element corresponds to a level of the section
        ## hierarchy. The last element applies to any lower levels
        ## present in the document. An empty list specifies no
        ## internal chunking.
        section_chunking = new_list_property(of = Chunking)
    )
)

RMarkdownChunking := new_class(MarkdownChunking)

QuartoChunking := new_class(MarkdownChunking)

chunk := new_generic(c("x", "by"))

method(chunk, list(class_any, NULL)) <- function(x, by) x

chunk_string_or_file <- function(x, by) {
    stopifnot(length(x) == 1L && !is.na(x))
    if (file.exists(x))
        x <- readLines(x) |> paste(collapse = "\n")
    chunk_string(x, by)
}

method(chunk, list(class_character, Chunking)) <- function(x, by) {
    chunks <- lapply(x, chunk_string_or_file, by)
    source <- names(x) %||% seq_along(x)
    data.frame(source = rep(source, sapply(chunks, nrow)),
               text = do.call(rbind, chunks))
}

chunk_string := new_generic(c("x", "by"))

chunk_starts <- function(by, len) {
    starts <- seq(1L, len, by = by@size - by@overlap)
    starts[len - starts >= by@overlap]
}

method(chunk_string, list(class_character, CharacterChunking)) <- function(x, by)
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

method(chunk_string, list(class_character, WordChunking)) <- function(x, by)
{
    chunk_elements(x, by, "\\s+")
}

method(chunk_string, list(class_character, ParagraphChunking)) <- function(x, by)
{
    chunk_elements(x, by, "\n(\\s*\n)+")
}

method(chunk_string, list(class_character, RMarkdownChunking)) <- function(x, by)
{
    stopifnot(length(x) == 1L && !is.na(x))
    require_ns("parsermd", "chunk RMarkdown")
    chunk(parsermd::parse_rmd(x), by)
}

method(chunk_string, list(class_character, QuartoChunking)) <- function(x, by)
{
    stopifnot(length(x) == 1L && !is.na(x))
    require_ns("parsermd", "chunk Quarto")
    chunk(parsermd::parse_qmd(x), by)
}

rmd_ast <- new_S3_class("rmd_ast")

method(chunk, list(rmd_ast, MarkdownChunking)) <- function(x, by) {
    df <- as.data.frame(x)
    leaf_types <- c("rmd_chunk", "rmd_markdown")
    df_leaves <- subset(df, type %in% leaf_types)
    grouping_cols <- intersect(names(df_leaves), paste0("sec_h", 1:6))
    section_chunking <- head(c(by@section_chunking, rep(list(NULL), 6L)), 6L)
    do.call(rbind, lapply(seq_along(grouping_cols), \(i) {
        grp <- head(grouping_cols, i)
        df_leaves[head(grp, -1L)] <- lapply(df_leaves[head(grp, -1L)], addNA)
        cdf <- aggregate(df_leaves["ast"], df_leaves[grp], \(ast) {
            chunk(paste(as_document(ast), collapse = "\n"),
                  section_chunking[[i]])
        }, simplify = FALSE)
        later_grp <- tail(grouping_cols, -i)
        data.frame(cdf[rep(seq_len(nrow(cdf)), lengths(cdf$ast)), grp],
                   sapply(later_grp, \(y) NA, simplify = FALSE),
                   text = unlist(cdf$ast))
    }))
}

chunk_Rmd <- function(x, section_chunking = list(ParagraphChunking())) {
    chunk(x, RMarkdownChunking(leaf_chunking = leaf_chunking))
}

chunk_Qmd <- function(x, section_chunking = list(ParagraphChunking())) {
    chunk(x, QuartoChunking(leaf_chunking = leaf_chunking))
}

chunk_Rd <- function(package) {
    chunk_Rd_db(tools::Rd_db(package))
}

chunk_Rd_db <- function(x) {
    chunks <- lapply(x, chunk_Rd_file)
    aliases <- vapply(x, \(f) paste(Rd_aliases(f), collapse = ","),
                      character(1L))
    section <- unlist(lapply(chunks, names), use.names = FALSE)
    data.frame(text = unlist(chunks, use.names = FALSE),
               aliases = rep(unname(aliases), lengths(chunks)),
               section = section)
}

Rd_sections <- c("description", "usage", "arguments", "details", "section",
                 "value", "examples")

chunk_Rd_file <- function(x) {
    names(x) <- gsub("\\", "", vapply(x, attr, character(1L), "Rd_tag"),
                     fixed = TRUE)
    c(file = Rd_src(x), vapply(x[names(x) %in% Rd_sections], Rd_src,
                               character(1L)))
}

Rd_aliases <- function(x) {
    unlist(Filter(\(xi) attr(xi, "Rd_tag") == "\\alias", x), use.names = FALSE)
}
