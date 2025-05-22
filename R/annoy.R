## Really an S4 reference class, but this keeps the dependency optional
Rcpp_AnnoyAngular <- setClass("Rcpp_AnnoyAngular", package = "RcppAnnoy")

annoy_index <- function(embedding, ntrees = ncol(embedding) %/% 5L) {
    require_ns("RcppAnnoy", "index vectors (by default)")
    index <- new(RcppAnnoy::AnnoyAngular, ncol(embedding))
    for (i in seq_len(nrow(embedding))) {
        index$addItem(i, embedding[i, ])
    }
    index$build(ntrees)
    index
}

method(retrieve, list(class_numeric, Rcpp_AnnoyAngular)) <- function(x, from,
                                                                     params) {
    nns <- from$getNNsByVectorList(x, params@k, -1L, include_distances = TRUE)
    cosine_similarity <- (2 - (nns$distance ^ 2)) / 2
    nns$item[cosine_similarity >= params@min_similarity]
}

annoy_path <- function(file) {
    paste0(tools::file_path_sans_ext(file), ".annoy")
}

method(on_persist, Rcpp_AnnoyAngular) <- function(x, file) {
    x$save(annoy_path(file))
}

method(on_restore, Rcpp_AnnoyAngular) <- function(x, file, ndim) {
    index <- new(RcppAnnoy::AnnoyAngular, ndim)
    index$load(annoy_path(file))
    index
}

method(param_class, Rcpp_AnnoyAngular) <- function(x) VectorIndexRetrievalParams
