\name{describe_with_Rd}
\alias{describe_with_Rd}

\title{
  Describing Tools with Rd
}
\description{
  Decorates a tool with information from its man page, assuming its
  underlying function comes from a package. The extracted information
  includes the description, argument (parameter) descriptions, and
  return value description.
}
\usage{
describe_with_Rd(tool)
}
\arguments{
  \item{tool}{
    The Tool object to describe.
  }
}
\details{
  To find the man page, the search algorithm first looks for a function
  with the tool's name in the enclosing environment of the underlying
  function. If not found, it linearly searches the enclosing environment
  for an \code{\link{identical}} function.
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
