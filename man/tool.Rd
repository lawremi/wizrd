\name{tool}
\alias{tool}

\title{
  Model Tools
}
\description{
  Convert an object, such as an R function or another model, into a tool
  for use by a model. The \code{\link{equip}} function performs this
  conversion automatically, but by calling \code{tool} directly, the
  user can customize the tool before equipping it.
}
\usage{
tool(x, name = deparse(substitute(x)), ...)
}
\arguments{
  \item{x}{Object to convert to a tool}
  \item{name}{Name of the new tool}
  \item{\dots}{
    Arguments passed to \code{\link[S7]{convert}}. Methods on
    \code{convert} typically forward these to the \code{Tool}
    constructor.
  }
}
\value{
  A Tool object
}
\author{Michael Lawrence}
\seealso{
  \code{\link{equip}} for equipping a model with a tool; functions like
  \code{\link{can_accept_as}}, \code{\link{demonstrate}} and
  \code{\link{describe_with_Rd}} for further specifying a tool
}
\examples{
\dontrun{
    model <- equip(llama(), tool(mean) |> describe_with_Rd())
}
}
\keyword{ utilities }
