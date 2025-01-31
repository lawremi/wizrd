\name{LanguageModelParams}
\alias{LanguageModelParams}
\title{
Language Model Parameters
}
\description{
  Constructs an object containing parameters for generating inferences
  from a model. The included parameters seem to be those held in common
  across most LLM implementations. Leaving a parameter at the default
  (\code{NULL}) inherits the default of the model (backend).
}
\usage{
LanguageModelParams(temperature = NULL, top_p = NULL, top_k = NULL, max_tokens = NULL, presence_penalty = NULL, frequency_penalty = NULL, stop = NULL)
}
\arguments{
  \item{temperature}{
    Sampling temperature, with higher being more random. Set to 0 for
    stable outputs.
  }
  \item{top_p}{
    Limit token selection to those contributing to the \code{top_p}
    fraction of probability mass.
  }
  \item{top_k}{
    Limit token selection to the \code{top_k} most probable.
  }
  \item{max_tokens}{
    Maximum limit on the total number of generated tokens.
  }
  \item{presence_penalty}{
    Positive numbers penalize tokens already appearing in the context,
    while negative numbers do the opposite. Typical range is between -2
    and 2.
  }
  \item{frequency_penalty}{
    Positive numbers penalize tokens that appear frequently in the
    context, while negative numbers do the opposite. Typical range is
    between -2 and 2.
  }
  \item{stop}{
    A character vector of strings that, when generated, cause the model
    to stop generating.
  }
}
\seealso{
  Typically the above arguments are passed to \code{\link{chat}} and
  \code{predict}. Constructing an instance of LanguageModelParams is
  more for advanced use.
}
\value{A LanguageModelParams object}
\author{
  Michael Lawrence
}
\keyword{ utilities }
