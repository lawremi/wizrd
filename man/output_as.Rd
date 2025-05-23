\name{output_as}
\alias{output_as}

\title{
  Output Constraints
}
\description{
  Configure a model to adhere to specific output constraints.
}
\usage{
output_as(x, format, ...)
}
\arguments{
  \item{x}{
    The Agent object
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
  \item{\dots}{
    Arguments passed to \code{convert} when converting \code{format} to
    a TextFormat.
  }
}
\value{
  The input Agent object except with new output constraints.
}
\author{Michael Lawrence}
\seealso{
  \code{\link{instruct}}, for generally instructing a
  model. \code{\link{system_prompt_as}} for more advanced system prompt
  construction.
}
\examples{
\dontrun{
    agent <- openai_agent("gpt-4o-mini", temperature = 0) |>
        instruct("Answer questions about this dataset:", mtcars)

    # Return a filtered subset of mtcars, which serves as the prototype
    filtered_agent <- agent |>
        output_as(mtcars)
    filtered_agent |>
        predict("Cars with mpg > 20")
    
    # Return a summary data.frame
    summary_agent <- agent |>
        output_as(data.frame(
            cyl = integer(),
            avg_mpg = numeric(),
            avg_hp = numeric()
        ))
    summary_agent |>
        predict("Average mpg and hp by number of cylinders.")
    
    # Using class_data.frame for more general output
    agent |>
        output_as(S7::class_data.frame) |>
        predict("Top 5 most fuel efficient cars including mpg, hp, and wt.")

    # Using a custom S7 class for more specialized data
    Person <- S7::new_class("Person", properties = list(
        first_name = S7::class_character,
        last_name = S7::class_character
    ))
    openai_agent(temperature = 0) |>
        output_as(Person) |>
        predict("Creators of R")
}
}
\keyword{ utilities }
