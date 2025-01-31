\name{last_output}
\alias{last_output}

\title{
  Getting the Last Model Output
}
\description{
  Convenience for extracting the last model output from a Chat
  object.
}
\usage{
last_output(x)
}
\arguments{
  \item{x}{
    The Chat object
  }
}
\value{
  The converted output (not necessarily the raw response).
}
\seealso{
  \code{\link[predict.LanguageModel]{predict}} is a convenience wrapper
  that uses this function.
}
\author{Michael Lawrence}
\examples{
\dontrun{
    last_output(chat(llama(), "Creators of R"))
}
}
\keyword{ utilities }
