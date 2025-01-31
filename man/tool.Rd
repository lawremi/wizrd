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
\details{
  Arguments passed to a tool are converted according to the types
  declared by \code{\link{accept_as}}, without any conversion by
  default.

  The call to the tool is evaluated in the \code{env} provided to
  \code{\link{chat}}, so any arguments converted to language objects
  will be evaluated as R code. If the model sends a backquoted string,
  like \code{`x`}, it will be converted to a name and resolved
  dynamically, as shown in the examples.

  The more annotations on a tool, the more the model will understand how
  to use it. \code{\link{describe_with_Rd}} is a convenient way to
  annotate a tool based on a packaged R function with information from
  its man page.
  
  There is nothing LLM-specific about Tool objects. Thus, they could
  even be used as a way to annotate ordinary R functions, like with
  inferred parameter types.
}
\author{Michael Lawrence}
\seealso{
  \code{\link{equip}} for equipping a model with a tool; functions like
  \code{\link{can_accept_as}}, \code{\link{demonstrate}} and
  \code{\link{describe_with_Rd}} for further specifying a tool
}
\examples{
\dontrun{
    var <- 1:10
    equip(llama(), tool(mean) |> describe_with_Rd()) |>
        predict("What is the mean of `var`?")
}
}
\keyword{ utilities }
