TextIndex <- new_class("TextIndex", abstract = TRUE)

EmbeddingTextIndex <- new_class("EmbeddingTextIndex", TextIndex,
                                properties = list(
                                    embedder = LanguageModel,
                                    vector_index_builder = new_property(
                                        class_function,
                                        default = quote(annoy_index)
                                    ),
                                    vector_index = class_any,
                                    ndim = nullable(prop_int_nn)
                                ))

resembles_chunks <- function(x) is.data.frame(x) && is.character(x$text)

TextStore <- new_class("TextStore",
                       properties = list(
                           index = TextIndex,
                           text = new_property(
                               class_data.frame,
                               setter = \(self, value) {
                                   if (!resembles_chunks(value))
                                       value <- chunk(value)
                                   if (!resembles_chunks(value))
                                       stop("'value' must be a data.frame and ",
                                            "have a 'text' column")
                                   if (identical(self@text, value))
                                       return(self)
                                   self@index <- build(self@index, value$text)
                                   self@text <- value
                                   self
                               }
                           )
                       ))

text_store <- function(index, text = NULL) {
    if (inherits(index, LanguageModel))
        index <- EmbeddingTextIndex(embedder = index)
    TextStore(index = index, text = text)
}

fetch <- new_generic("fetch", c("x", "from"))

method(fetch, list(class_any, TextStore)) <- function(x, from, params) {
    from@text[fetch(x, from@index, params),]
}

method(fetch, list(class_any, EmbeddingTextIndex)) <- function(x, from, params) {
    embeddings <- embed_text(from@embedder, x)
    unique(unlist(apply(embeddings, 1L, fetch, from@vector_index, params,
                        simplify = FALSE)))
}

build <- new_generic("build", "x")

method(build, TextStore) <- function(x, text, ...) {
    set_props(x, text = text)
}

method(build, EmbeddingTextIndex) <- function(x, text, ...) {
    if (is.null(text))
        return(x)
    embedding <- embed_text(x@embedder, text, x@ndim)
    set_props(x, vector_index = x@vector_index_builder(embedding, ...),
              ndim = ncol(embedding))
}

VectorIndexRetrievalParams := new_class(
    properties = list(
        k = new_int_property(min = 0L, default = 5L),
        min_similarity = new_number_property(min = 0L)
    )
)

## Is this the right way to do RAG? Or should we provide the model a
## tool that performs a search?
RetrievalAugmentedFormat <- new_class("RetrievalAugmentedFormat", TextFormat,
                                      properties = list(
                                          store = TextStore,
                                          params = VectorIndexRetrievalParams
                                      ))

param_class := new_generic("x")

method(convert, list(class_any, TextStore)) <- function(from, to) {
    TextStore(nomic(), from)
}

rag_from <- function(store, k = 5L, min_similarity = 0L, ...) {
    store <- convert(store, TextStore)
    params <- param_class(store@index@vector_index)(k = as.integer(k),
        min_similarity = min_similarity, ...)
    RetrievalAugmentedFormat(store = store, params = params)
}

method(textify,
       list(class_character | class_list | class_any,
            RetrievalAugmentedFormat)) <- function(x, format)
{
    results <- fetch(x, format@store, format@params)
    paste0("Using this information:\n",
           textify(results),
           "\n\nRespond to this prompt:\n",
           textify(x))
}

method(on_restore, EmbeddingTextIndex) <- function(x, file) {
    set_props(x,
              embedder = on_restore(x@embedder),
              vector_index = on_restore(x@vector_index, file, x@ndim))
}

method(print, EmbeddingTextIndex) <- function(x, ...) {
    cat(x@embedder@name %||% "<unnamed>", " -> <",
        class(x@vector_index)[1L], "> (x", x@ndim, ")\n", sep = "")
}

method(print, TextStore) <- function(x, ...) {
    cat(S7:::obj_desc(x))
    cat("\n")
    cat("@index: "); print(x@index)
    cat("@text:", if (!is.null(x@text)) nrow(x@text) else 0L, "records")
    cat("\n")
}
