\name{rag_from}
\alias{rag_from}

\title{
  Retrieval Augmented Generation
}
\description{
  Configure a model to perform retrieval-augmented generation (RAG)
  using the specified database and parameters. RAG augments the model
  prompt with the result of a query to a text store, containing
  annotated text chunks, so that the model's response considers both the
  query and the retrieved records.
}
\usage{
rag_from(store, k = 5L, min_similarity = 0L, ...)
}
\arguments{
  \item{store}{
    A TextStore object to be queried for text chunks based on the user
    input and its index, which determines the retrieval algorithm.
  }
  \item{k}{The maximum number of chunks to retrieve}
  \item{min_similarity}{
    The minimum similarity for a chunk to match a query. The default
    index uses cosine similarity.
  }
  \item{\dots}{Additional parameters controlling the retrieval algorithm}
}
\value{
  A TextFormat object to be used as the prompt format for the model by
  passing it to \code{\link{prompt_as}}
}
\author{Michael Lawrence}
\seealso{
  \code{\link{prompt_as}}, to which the return value of \code{rag_from}
  should be passed in order to configure a model to use RAG.
}
\note{
  The RAG capabilities of \pkg{wizrd} are currently experimental and
  quite primitive. They may be removed or moved to a different package in
  the future.
}
\examples{
\dontrun{
    chunks <- chunk(tools::Rd_db("S7"))
    store <- text_store(nomic(), chunks)
    model <- llama() |> prompt_as(rag_from(store))
    predict(model, "new_property example")
}
}
\keyword{ utilities }
