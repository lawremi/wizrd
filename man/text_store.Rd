\name{text_store}
\alias{text_store}

\title{
  Text Stores
}
\description{
  Build a store of text chunks, e.g. for use in RAG. A TextStore binds a
  collection of annotated text chunks to an index that optimizes
  queries.
}
\usage{
text_store(index, text = NULL)
}
\arguments{
  \item{index}{
    A TextIndex object or, more commonly, an Agent object for
    computing the embeddings using the default index implementation.
  }
  \item{text}{
    Optionally, a data.frame with a column named \dQuote{text} that
    contains the text chunks. The other columns in the data.frame are
    stored along with the chunks as metadata. Query results are returned
    as entire rows, including both the text and metadata. To set the
    text later, assign to the \code{@text} property on the returned
    TextStore.
  }
}
\value{
  A TextFormat object to be used as the prompt format for the agent by
  passing it to \code{\link{prompt_as}}.
}
\author{Michael Lawrence}
\seealso{
  \code{\link{retrieve}} for retrieving text chunks from a
  store. \code{\link{rag_with}}, for using the returned object in
  RAG. \code{persist} and \code{restore} for persistence of text stores.
}
\note{
  The RAG capabilities of \pkg{wizrd} are currently experimental and quite
  primitive. They may be removed or moved to a different package in
  the future.
}
\details{
  The default index uses an embedding agent to embed the chunks in an
  N-dimensional space and indexes the embedding using the Annoy
  (Approximate Nearest Neighbors Oh Yeah) algorithm. Nearest neighbors
  are resolved using cosine similarity.
}
\examples{
\dontrun{
    chunks <- chunk(tools::Rd_db("S7"))
    store <- text_store(nomic(), chunks)
    agent <- llama() |> prompt_as(rag_with(store))
    predict(agent, "new_property example")
}
}
\keyword{ utilities }
