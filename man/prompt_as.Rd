\name{prompt_as}
\alias{prompt_as}
\alias{system_prompt_as}

\title{
  Prompt Construction
}
\description{
  Configure a model to be prompted in a systematic way.
}
\usage{
prompt_as(x, format, ...)
system_prompt_as(x, format, ...)
}
\arguments{
  \item{x}{
    The Agent object
  }
  \item{format}{
    A TextFormat object, or something convertible to one, representing
    the prompt format. A function is converted to a TextFormat that
    expects a list of arguments to be passed as input. The arguments are
    passed to the function, and the return value is sent to the
    model. If this argument is a string, the system will try
    to interpret it as either a file, glue format string or
    \href{https://smith.langchain.com/hub}{Langsmith Hub} ID. If it
    resembles a file, the file is read as a string and treated as a glue
    template. If it is a Langsmith Hub ID, the corresponding prompt
    template is downloaded and set on the model (including the system
    prompt). Both fstring (via \pkg{glue}) and mustache (via
    \pkg{whisker}) templates are supported from the hub.
  }
  \item{\dots}{
    Arguments passed to methods
  }
}
\details{
  The specified \code{format} object controls (via dispatch) how the
  user input is converted to a textual prompt. In most cases, templating
  is sufficient, and the user can pass template parameters as a list (or
  similar object, like an environment) as the \code{input} argument to
  e.g. \code{\link{chat}}. An example of a more complex conversion is
  Retrieval Augmented Generation (RAG), via \code{\link{rag_with}}.

  When passing Langsmith Hub IDs, note that \code{prompt_as} is
  considered the more general entry point, supporting chat and
  structured output prompt templates in addition to basic prompt
  templates. \code{system_prompt_as} only supports basic prompt
  templates, since it can only modify the system prompt.
}
\value{
  The input Agent object except with a new prompt format.
}
\author{Michael Lawrence}
\examples{
\dontrun{
    llama() |>
        prompt_as("Output a {nchar}-letter word starting with '{initial}'.") |>
        predict(list(nchar = 4L, initial = "a"))

    llama() |>
        system_prompt_as("Answer questions about {language}") |>
        chat(system_params = list(language = "R")) |>
        predict("Who created the language?")
}
}
\keyword{ utilities }
