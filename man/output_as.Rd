\name{output_as}
\alias{output_as}

\title{
  Output Constraints
}
\description{
  Configure a model to adhere to specific output constraints.
}
\usage{
output_as(x, format)
}
\arguments{
  \item{x}{
    The LanguageModel object
  }
  \item{format}{
    A TextFormat object, or something convertible to one, representing
    the output constraints. For anything more complex than a string,
    the output will typically need to be JSON, with constraints
    expressed as JSON schema. While it is possible to pass a list
    directly encoding a JSON schema, it is usually more convenient to
    pass a stubbed data.frame (for data.frame outputs) or an S7 class,
    in which case the output is an instance of the class.
  }
}
\value{
  The input LanguageModel object except with new output constraints.
}
\author{Michael Lawrence}
\seealso{
  \code{\link{instruct}}, for generally instructing a
  model. \code{\link{system_prompt_as}} for more advanced system prompt
  construction.
}
\examples{
\dontrun{
    df <- data.frame(first_name = character(), last_name = character())
    openai_model(temperature = 0) |> output_as(df) |> predict("Creators of R")

    Person <- S7::new_class("Person", properties = list(
        first_name = S7::class_character,
        last_name = S7::class_character
    ))
    openai_model(temperature=0) |> output_as(Person) |> predict("Creators of R")
}
}
\keyword{ utilities }
