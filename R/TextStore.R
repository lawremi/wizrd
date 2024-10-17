TextIndex <- new_class("TextIndex",
                       properties = list(
                           embedder = LanguageModel,
                           vector_index_builder = new_property(
                               class_function,
                               default = annoy_index
                           ),
                           vector_index = class_any,
                           ndim = nullable(prop_int_nn)
                       ))

TextStore <- new_class("TextStore",
                       properties = list(
                           index = TextIndex,
                           text = new_property(
                               NULL | class_data.frame,
                               setter = \(self, value) {
                                   if (!is.null(value) && is.null(value$text))
                                       stop("'value' must have a 'text' column")
                                   self@index <- build(self@index, value$text)
                                   self@text <- value
                                   self
                               }
                           )
                       ))

text_store <- function(index, text = NULL) {
    if (inherits(index, LanguageModel))
        index <- TextIndex(embedder = index)
    TextStore(index = index, text = text)
}

method(fetch, list(class_any, TextStore)) <- function(x, from, n) {
    x@text[fetch(x, from@index, n),]
}

method(fetch, list(class_any, TextIndex)) <- function(x, from, n) {
    embed_text(from@embedder, x) |> fetch(from@vector_index, n)
}

build <- new_generic("build", "x")

method(build, TextStore) <- function(x, text, ...) {
    set_props(x, text = text)
}

method(build, TextIndex) <- function(x, text, ...) {
    if (is.null(text))
        return(x)
    embedding <- embed_text(x@embedder, text, x@ndim)
    set_props(x, vector_index = x@vector_index_builder(embedding, ...),
              ndim = ncol(embedding))
}

ResultsAugmentedFormat <- new_class("ResultsAugmentedFormat", PlainTextFormat,
                                    properties = list(
                                        store = TextStore,
                                        n = new_int_property(
                                            min = 0L,
                                            default = 5L
                                        )
                                    ))

results_augmented_query_to <- function(store, n = 5L) {
    assert_int(n, lower = 0L)
    ResultsAugmentedFormat(store = store, n = as.integer(n))
}

method(textify, list(class_any, ResultsAugmentedFormat)) <- function(x, format) {
    results <- fetch(x, format@store, format@n)
    paste0("Using this data:\n",
           textify(results),
           "\n\nRespond to this prompt:\n",
           textify(x))
}

method(on_restore, TextIndex) <- function(x, file) {
    set_props(x, vector_index = on_restore(x@vector_index, file, x@ndim))
}
