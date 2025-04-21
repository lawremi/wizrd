\name{embed_text}
\alias{embed_text}

\title{
  Text Embedding
}
\description{
  Generates embeddings of text chunks as a matrix.
}
\usage{
embed_text(x, text, ndim, ...)
}
\arguments{
  \item{x}{
    The Agent object, whose underlying model should be
    specialized for generating embeddings, like \code{\link{nomic}}.
  }
  \item{text}{
    A data.frame, typically generated via \code{\link{chunk}}, with
    a \dQuote{text} column containing text chunks to embed.
  }
  \item{ndim}{
    The dimensionality of the embedding (the length of the each
    embedding vector). If \code{NULL}, it uses the model default.
  }
  \item{\dots}{
    Arguments passed to the embedding backend
  }
}
\value{
  A matrix containing embedding coordinates, with one row per row of
  \code{data} and \code{ndim} columns.
}
\seealso{
  For convenience, just pass the text chunks to
  \code{\link{text_store}}, which automatically generates embeddings by
  default.
}
\author{Michael Lawrence}
\examples{
\dontrun{
    chunks <- chunk(tools::Rd_db("S7"))
    embed_text(nomic(), chunks)
}
}
\keyword{ utilities }
