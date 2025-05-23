\name{rag_with}
\alias{rag_with}

\title{
  Retrieval Augmented Generation (RAG)
}
\description{
  Create a text format that augments prompts with relevant text chunks
  retrieved from a text store.
}
\usage{
rag_with(store, k = 5L, min_similarity = 0L, ...)
}
\arguments{
  \item{store}{
    A \code{TextStore} object containing the text chunks to search.
  }
  \item{k}{
    The number of chunks to retrieve.
  }
  \item{min_similarity}{
    The minimum similarity score for a chunk to be included.
  }
  \item{\dots}{
    Additional arguments passed to the vector index.
  }
}
\value{
  A \code{RetrievalAugmentedFormat} object that can be passed to
  \code{\link{prompt_as}}, to which the return value of \code{rag_with}
  is passed.
}
\author{Michael Lawrence}
\seealso{
  \code{\link{prompt_as}}, to which the return value of \code{rag_with}
  should be passed in order to configure an agent to use RAG.
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
    agent <- llama() |> prompt_as(rag_with(store))
    predict(agent, "new_property example")
}
}
\keyword{ utilities } 