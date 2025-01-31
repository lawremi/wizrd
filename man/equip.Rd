\name{equip}
\alias{equip}
\alias{unequip}

\title{
  Equipping Tools
}
\description{
  Configure a model call (or not) a tool.
}
\usage{
equip(x, tool, instructions = NULL, ...)
unequip(x, name)
}
\arguments{
  \item{x}{
    The LanguageModel object
  }
  \item{tool}{The Tool object, or an object to convert to a Tool object}
  \item{instructions}{
    A string instructing the model on how and when to use the
    tool. Typically, this is not necessary, because the Tool object is
    sufficiently described. This is intended to allow a Tool to be used
    in different ways in different contexts.
  }
  \item{name}{The name of the tool to unequip}
  \item{\dots}{
    Arguments passed to \code{\link{tool}} when ensuring that
    the \code{tool} argument is a Tool object. Typically, these get
    forwarded to the \code{Tool} constructor.
  }
}
\value{
  The input LanguageModel object except with a different set of tools.
}
\author{Michael Lawrence}
\seealso{
  \code{\link{tool}}, for constructing (and configuring) Tool objects
  prior to equipping.
}
\examples{
\dontrun{
    model <- equip(llama(), tool(mean) |> describe_with_Rd())
}
}
\keyword{ utilities }
