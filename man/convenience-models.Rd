\name{convenience-models}
\alias{test_model}
\alias{quick_start_model}
\alias{vision_model}
\alias{embedding_model}

\title{
  Convenience Functions for Common Models
}
\description{
  These functions provide convenient access to local models for specific purposes.
}
\usage{
test_model(server = ollama_server(), temperature = 0, ...)
quick_start_model(temperature = 0, ...)
vision_model(server = ollama_server(), temperature = 0, ...)
embedding_model(server = ollama_server(), temperature = 0, ...)
}
\arguments{
  \item{server}{
    The Ollama-based models require an Ollama server to be running. By
    default, this calls \code{\link{ollama_server}} to start the server,
    if it is not already running. See the cautionary note in
    \code{\link{ollama_agent}} for the potential pitfalls of this
    convenience.
  }
  \item{temperature}{
    The temperature of the model, defaulting to zero for convenient
    stability during testing and demos.
  }
  \item{...}{
    Additional model parameters, see
    \code{\link{LanguageModelParams}}.
  }
}
\details{
  \code{test_model}, \code{vision_model} and \code{embedding_model} are
  all pulled from
  Ollama, so Ollama must be installed.

  \code{quick_start_model} is meant as a quick start. It will download and
  cache a self-contained, cross-platform llamafile binary. No Ollama required.
}
\section{Issues with llamafile}{
  See \code{\link{llama_cpp_agent}} for resolving OS-specific issues
  with \code{quick_start_model}.
}
\value{
  An Agent
}
\author{Michael Lawrence}
\examples{
\dontrun{
    quick_start_model() |>
        instruct("Answer questions about the mtcars dataset:", mtcars) |> 
        predict("Relationship between hp and fuel efficiency")
}
}
\keyword{ utilities }
