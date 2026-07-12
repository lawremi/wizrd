\name{stop_model}
\alias{stop_model}
\title{Stop a local model process}
\usage{
stop_model(x)
}
\arguments{
    \item{x}{An agent, chat, pipeline, or model backend.}
}
\description{
    Stops an associated local model process, if one exists and is still running.
    This is useful for helpers such as \code{quick_start_model()} that start a
    local server process.
}
\value{
    Invisibly returns \code{TRUE} if a process was stopped, otherwise
    \code{FALSE}.
}
\examples{
\dontrun{
agent <- quick_start_model()
stop_model(agent)
}
}
