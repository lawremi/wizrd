\name{convenience-models}
\alias{llama}
\alias{llamafile_llama}
\alias{llama_vision}
\alias{nomic}

\title{
  Convenience Models
}
\description{
  These are shortcuts for installing and loading some general, small
  models for the purpose of testing and demonstration.
}
\usage{
llama(temperature = 0, ...)
llamafile_llama(temperature = 0, ...)
llama_vision(temperature = 0, ...)
nomic(temperature = 0, ...)
}
\arguments{
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
  See \code{\link{llama_cpp_model}} for resolving OS-specific issues
  with \code{llamafile_llama}.
}
\value{
  A LanguageModel
}
\author{Michael Lawrence}
\examples{
\dontrun{
    llamafile_llama() |> predict("Creators of R")
}
}
\keyword{ utilities }
