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
