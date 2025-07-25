\name{instruct}
\alias{instruct}

\title{
  Instructing Models
}
\description{
  Configure a model to behave a certain way.
}
\usage{
instruct(x, ...)
}
\arguments{
  \item{x}{
    The Agent object
  }
  \item{\dots}{
    Objects that are converted to text and pasted together into a block
    of instructions for the model.
  }
}
\value{
  The input Agent object except with a new set of instructions.
}
\author{Michael Lawrence}
\seealso{
  \code{\link{output_as}}, for instructing a model on its output
  format. \code{\link{system_prompt_as}} for more advanced system prompt
  construction.
}
\examples{
\dontrun{
    llama() |> instruct("Return only first names") |> predict("Creators of R")
}
}
\keyword{ utilities }
