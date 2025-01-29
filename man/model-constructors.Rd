\name{model-constructors}
\alias{azure_openai_model}
\alias{llama_cpp_model}
\alias{ollama_model}
\alias{openai_model}

\title{
  LanguageModel Constructors
}
\description{
  Convenience constructors for LanguageModel objects based on different
  backends, such as Ollama or OpenAI. Users should call one of these
  functions to gain access to an LLM. For the local backends, every
  attempt is made to install the model and even the backend in some
  cases, so that users can call these functions in a script
  and expect the script to run anywhere, more or less reproducibly.
}
\usage{
ollama_model(name, pull = NA, server = ollama_server(), ...)
llama_cpp_model(path, mode = c("chat", "embedding"), alias = NULL,
                server_path = NULL, ...)
openai_model(name = "gpt-4o-mini", ...)
azure_openai_model(name = "gpt-4o", ...)
}

\arguments{
  \item{name}{
    Name of the model as identified by the backend
  }
  \item{pull}{
    Whether to pull the model from Ollama's repository. If \code{NA}
    (the default), prompt the user, except when the session is
    non-interactive, in which case \code{TRUE} is assumed.
  }
  \item{server}{
    OllamaServer object. By default, looks for a server running on
    localhost on the default port. If no server is found, attempts to
    start the server.
  }
  \item{path}{
    A string pointing to a model file with a format that is compatible
    with llama.cpp, or a
    \href{https://github.com/Mozilla-Ocho/llamafile}{llamafile}. This
    can be a simple path on the file system, a URL or the name of an
    Ollama model. If it resembles a URL, the file is downloaded and
    cached in a standard place (see \code{\link[tools]{R_user_dir}}). If
    it resembles an Ollama identifier, the path to the model is
    automatically resolved, pulling the model if necessary.
  }
  \item{mode}{
    Whether the model is to be used for \code{"chat"} or
    \code{"embedding"}. Embedding models are accessed via the llamafile
    "v2" web server, while chat models are run through the default
    llama.cpp web server.
  }
  \item{alias}{
    An alternative, more human-friendly name for the model, instead of
    using the \code{path}.
  }
  \item{server_path}{
    The path to the llama.cpp server binary. Ignored if \code{path} is a
    (self-contained) llamafile. Otherwise, defaults to the automatic,
    user-local installation of the llamafile utility, downloading and
    installing it if necessary and (when interactive) the user approves.
  }
  \item{\dots}{
    Model parameters, see \code{\link{LanguageModelParams}}.
  }
}
\value{A LanguageModel object}
\author{Michael Lawrence}
\note{
  These functions rely on an unexported lower level API that will
  eventually be exported to enable extension to additional backends.
}
\seealso{
  \code{\link[=predict.LanguageModel]{predict}} and \code{\link{chat}}
  for interacting with the returned LanguageModel object.
}
\examples{
\dontrun{
    model <- ollama_model("llama3.2:3b-instruct-q4_K_M") # or llama()
    predict(model, "Creators of R")
}
}
\keyword{ utilities }
