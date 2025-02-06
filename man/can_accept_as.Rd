\name{can_accept_as}
\alias{can_accept_as}

\title{
  Declaring Tool Parameters
}
\description{
  Declares the tool parameters and their classes. The model is expected
  to only provide the arguments specified, and \pkg{wizrd} will attempt
  to convert whatever the model sends to the declared class.
}
\usage{
can_accept_as(x, ..., `_parameters` = list(...))
}
\arguments{
  \item{x}{
    The Tool object
  }
  \item{\dots}{
    Each argument is any S7 class object, including base and S3 classes,
    unions and the wildcard \code{\link[S7]{S7_any}}. The arguments are
    matched to the underlying function via \code{\link{match.call}},
    i.e. normal R calling semantics.
  }
  \item{_parameters}{
    Convenience for providing a list instead of parameter classes
    instead of individual args in \code{\dots}.
  }
}
\details{
  By default (if this function is never called), the formals of the
  underlying function are taken as the parameters, and each accepts any
  value. \code{\dots}, if present, is declared as a list.
  
  The model is expected to send only the declared parameters, with some
  potentially being captured by \code{\dots} in the underlying
  function. To request that the model send extra arguments (as a list)
  that would be captured by \code{\dots}, pass an argument named
  \code{"\dots"}, e.g. \code{accept_as(x, "..." = class_list)}, which is
  the default behavior as mentioned above.

  The arguments are evaluated in the \code{env} passed to
  \code{\link{chat}}, so declaring an argument as a language object
  (\code{class_name} or \code{class_call}) will evaluate the argument as
  R code. See the below example for how this could be useful.
}
\value{
  The input Tool object except with newly declared parameters and types.
}
\author{Michael Lawrence}
\examples{
\dontrun{
    var <- 1:10
    model <- llama() |>
        equip(tool(mean) |> describe_with_Rd() |> can_accept_as(class_name)) |>
        predict("What is the mean of var?")
}
}
\keyword{ utilities }
