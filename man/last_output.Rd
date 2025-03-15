\name{last_output}
\alias{last_output}
\alias{last_message}

\title{
  Getting the Last Model Output
}
\description{
  Convenience for extracting the last model output from a Chat
  object.
}
\usage{
last_output(x)
last_message(x, role = NULL)
}
\arguments{
  \item{x}{
    The Chat object
  }
  \item{role}{
    The role (\code{"assistant"}, \code{"user"}, \code{"tool"},
    \code{"system"}, etc) of the message source; \code{NULL} represents
    any role
  }
}
\value{
  For \code{last_output}, the converted output (not necessarily the raw
  response). For the lower level \code{last_message}, a ChatMessage
  object containing the raw and converted output, among other things.
}
\seealso{
  \code{\link[=predict.LanguageModel]{predict}} is a convenience wrapper
  that uses \code{last_output}.
}
\author{Michael Lawrence}
\examples{
\dontrun{
    last_output(chat(llama(), "Creators of R"))
}
}
\keyword{ utilities }
