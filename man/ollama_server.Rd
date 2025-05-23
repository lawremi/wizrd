\name{ollama_server}
\alias{ollama_server}

\title{
  Construct an Ollama Server Object
}
\description{
  Constructs a reference to an Ollama server, optionally starting one if
  one is not running at the specified (local) URL. If the server is
  started, it will be killed when the returned object goes out of scope.

  This function requires Ollama to be installed on your system. You can
  install Ollama from \url{https://ollama.ai} and follow their
  platform-specific installation instructions.
}
\usage{
ollama_server(url = ollama_url(), start = TRUE, ...)
}

\arguments{
  \item{url}{
    URL to the server, taken as the environment variable
    \env{OLLAMA_HOST} or \url{http://127.0.0.1:11434} by default.
  }
  \item{start}{
    Whether to start the server if one is not running.
  }
  \item{\dots}{
    Arguments passed to an underlying function.
  }
}
\value{An OllamaServer object}
\author{Michael Lawrence}
\examples{
\dontrun{
    server <- ollama_server() # keep a reference to this object
    agent <- llama(server)
    agent |>
        instruct("Answer questions about this dataset:", mtcars) |>
        predict("What is the relationship between horsepower and mpg?")
}
}
\keyword{ utilities }
