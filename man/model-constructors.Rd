\name{model-constructors}
\alias{azure_openai_agent}
\alias{llama_cpp_agent}
\alias{ollama_agent}
\alias{openai_agent}

\title{
  Agent Constructors
}
\description{
  Convenience constructors for Agent objects based on different
  backends, such as Ollama or OpenAI. Users should call one of these
  functions to gain access to an LLM. For the local backends, every
  attempt is made to install the model and even the backend in some
  cases, so that users can call these functions in a script
  and expect the script to run anywhere, more or less reproducibly.
}
\usage{
ollama_agent(name, pull = NA, server = ollama_server(), ...)
llama_cpp_agent(path, mode = c("chat", "embedding"), alias = NULL,
                server_path = NULL, ...)
openai_agent(name = "gpt-4o-mini", ...)
azure_openai_agent(name = "gpt-4o", url = getOption("wizrd_azure_openai_url"),
                   ...)
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
    start the server. Note that if this ends up starting the server, the
    server process will be killed when the returned object goes out of
    scope. Thus, it is safest to start the server explicitly using
    \code{\link{ollama_server}} and keep a reference around (or just run
    \command{ollama serve} in a shell).
  }
  \item{path}{
    A string pointing to a
    \href{https://github.com/Mozilla-Ocho/llamafile}{llamafile} or a
    file containing weights in a format compatible with
    llama.cpp. This can be a simple path on the file system, a URL or
    the name of an Ollama model. If it resembles a URL, the file is
    downloaded and cached in a standard place (see
    \code{\link[tools]{R_user_dir}}). If it resembles an Ollama
    identifier, the path to the model is automatically resolved, pulling
    the model if necessary.
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
  \item{url}{The URL of the Azure endpoint, typically ending with the hostname.}
  \item{\dots}{
    Model parameters, see \code{\link{LanguageModelParams}}.
  }
}

\section{Commercial Providers}{
  Using a commercial provider, like via \code{openai_agent} or
  \code{azure_openai_agent}, requires some configuration, depending on
  the provider:
  \describe{
    \item{openai_agent}{\var{OPENAI_API_KEY} holds the API key}
    \item{azure_openai_agent}{
      \var{AZURE_OPENAI_API_KEY} holds the API
      key, and \code{option(wizrd_azure_openai_url=)} sets the
      account-specific URL (up to the hostname).
    }
  }
}

\section{Issues with llamafile}{
  While a llamafile is supposed to work seamlessly across platforms,
  OS-specific issues may arise. Examples include:
  \itemize{
    \item{On Linux, issues with binary format registration may result in
      a wine popup appearing when attempting to run a llamafile.
    }
    \item{On Windows, llamafiles over 4GB in size are not supported.}
  }
  See the \href{https://github.com/Mozilla-Ocho/llamafile}{llamafile
    README} for workarounds.
}
\value{An Agent object}
\author{Michael Lawrence}
\note{
  These functions rely on an unexported lower level API that will
  eventually be exported to enable extension to additional backends.
}
\seealso{
  \code{\link[=predict.Agent]{predict}} and \code{\link{chat}}
  for interacting with the returned Agent object.
}
\examples{
\dontrun{
    model <- ollama_agent("llama3.2:3b-instruct-q4_K_M") # or llama()
    model |>
        instruct("Answer questions about this dataset:", mtcars) |>
        predict("What is the relationship between horsepower and mpg?")
}
}
\keyword{ utilities }
