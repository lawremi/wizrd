\name{demonstrate}
\alias{demonstrate}
\alias{demonstrate_all}

\title{
  Demonstrating with Examples
}
\description{
  Decorates a tool or model with one or more examples, which is an
  effective way to ensure correct behavior.
}
\usage{
demonstate(x, input, output)
demonstate_all(x, examples)
}
\arguments{
  \item{x}{
    The Tool or LanguageModel object.
  }
  \item{input,output}{
    Input and output objects constituting an example. They are assumed
    to be scalar and, if non-atomic, are wrapped in a list when being
    combined with the data.frame of existing examples. To add multiple
    examples at once, use \code{demonstrate_all}.
  }
  \item{examples}{
    A data.frame with an \dQuote{input} and \dQuote{output} column
    containing input and output objects, respectively.
  }
}
\details{
  The input and output objects for each example are converted to/from
  text in the same way as the actual input and output. For models, those
  mappings are configured by \code{\link{prompt_as}} and
  \code{\link{output_as}}. For tools, the input should be a list of
  arguments corresponding to the function formals, modulo any
  customization via \code{\link{accept_as}}. The tool output is probably
  best described as a string, as anything more complex, like JSON
  schema, tends to confuse models.
}
\value{
  The input object except with newly declared examples.
}
\author{Michael Lawrence}
\examples{
\dontrun{
    aggregate_tool <- tool(aggregate) |>
        can_accept_as(x = class_formula, data = class_name, FUN = class_name) |>
        demonstrate(c(x = MPG.city ~ Origin, alist(data = Cars93, FUN = median)),
                    "the median of MPG.city by Origin in Cars93")
    model <- equip(llama(), aggregate_tool)
    data(Cars93, package = "MASS")
    predict(model,
            "Mean of MPG.city for each Manufacturer in the Cars93 dataset")
}
}
\keyword{ utilities }
