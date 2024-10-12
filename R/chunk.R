chunk_Rd <- function(package) {
    chunk_Rd_db(tools::Rd_db(package))
}

chunk_Rd_db <- function(x) {
    chunks <- lapply(x, chunk_Rd_file)
    aliases <- lapply(x, \(f) paste(Rd_aliases(f), collapse = ","))
    section <- unlist(lapply(chunks, names))
    data.frame(text = unlist(chunks),
               aliases = rep(aliases, lengths(chunks)),
               section = section)
}

Rd_sections <- c("description", "usage", "arguments", "details", "section",
                 "value", "examples")

chunk_Rd_file <- function(x) {
    names(x) <- vapply(x, attr, character(1L), "Rd_tag")
    c(file = Rd_src(x), vapply(x[Rd_sections], Rd_src, character(1L)))
}

Rd_aliases <- function(x) {
    unlist(Filter(\(xi) attr(xi, "Rd_tag") == "alias"))
}
