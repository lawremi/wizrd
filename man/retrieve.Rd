\name{retrieve}
\alias{retrieve}

\title{
  Retrieve Documents
}
\description{
  Retrieve documents (chunks of text) from a text store.
}
\usage{
retrieve(x, from, ...)
}
\arguments{
  \item{x}{
    The query, which by default is embedded and used as a query to a
    vector store based on cosine similarity.
  }
  \item{from}{
    The TextStore as returned by \code{\link{text_store}}.
  }
  \item{\dots}{
    Parameters for the query. By default, the supported parameters are
    \code{k} (the number of nearest neighbor documents to return) and
    \code{min_similarity} (the minimum allowed cosine similarity of the
    returned documents to the query).
  }
}
\value{
  A data.frame with a column \dQuote{text} containing the text of each
  document, along with any metadata columns.
}
\author{Michael Lawrence}
\seealso{
  \code{\link{rag_with}}, which implements the full RAG workflow.
}
\note{
  The RAG capabilities of \pkg{wizrd} are currently experimental and quite
  primitive. They may be removed or moved to a different package in
  the future.
}
\examples{
\dontrun{
    chunks <- chunk(tools::Rd_db("S7"))
    store <- text_store(nomic(), chunks)
    retrieve("new_property example", store)
}
}
\keyword{ utilities }
