\name{persistence}
\alias{persist}
\alias{restore}

\title{
  Object Persistence
}
\description{
  This is an experimental, dispatch-based mechanism for customized
  (de)serialization of R objects. The driving use case is persistence of
  TextStore objects, which will often reference externally persisted
  indices.
}
\usage{
persist(x, file)
restore(file)
}
\arguments{
  \item{x}{
    The object being persisted.
  }
  \item{file}{
    The file holding the (partially) serialized R object. \code{persist}
    may create additional files, which will be required by
    \code{restore} to fully restore the object.
  }
}
\value{
  For \code{restore}, the fully restored object.
}
\author{Michael Lawrence}
\examples{
\dontrun{
    chunks <- chunk(tools::Rd_db("S7"))
    store <- text_store(nomic(), chunks)
    file <- tempfile("text", fileext = ".rds")
    persist(store, file)
    store <- restore(file)
    model <- llama() |> prompt_as(rag_from(store))
    predict(model, "new_property example")
}
}
\keyword{ utilities }
