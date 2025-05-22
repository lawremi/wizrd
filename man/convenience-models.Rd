\name{convenience-models}
\alias{llama}
\alias{llamafile_llama}
\alias{llama_vision}
\alias{nomic}

\title{
  Convenience Functions for Common Models
}
\description{
  These functions provide convenient access to common local models.
}
\usage{
llama(server = ollama_server(), temperature = 0, ...)
llamafile_llama(temperature = 0, ...)
llama_vision(server = ollama_server(), temperature = 0, ...)
nomic(server = ollama_server(), temperature = 0, ...)
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
  \code{llama}, \code{llama_vision} and \code{nomic} are all pulled from
  Ollama, so Ollama must be installed.

  \code{llamafile_llama} is meant as a quick start. It will download and
  cache a self-contained, cross-platform llamafile binary using the same
  weights as \code{llama}. No Ollama required.
}
\section{Issues with llamafile}{
  See \code{\link{llama_cpp_agent}} for resolving OS-specific issues
  with \code{llamafile_llama}.
}
\value{
  An Agent
}
\author{Michael Lawrence}
\examples{
\dontrun{
    llamafile_llama() |>
        instruct("Answer questions about the mtcars dataset:", mtcars) |> 
        predict("Relationship between hp and fuel efficiency")
}
}
\keyword{ utilities }
