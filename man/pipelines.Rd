\name{pipelines}
\alias{c.ChatPipeline}
\alias{c.Chat}
\alias{c.Agent}

\title{
  Chat Pipelines
}
\description{
  Agent and Chat objects can be combined into ChatPipeline objects using
  \code{c}. Chatting to a pipeline, via \code{\link{chat}}, sends the
  input to the first element of the pipeline. The output from that
  element is sent to the second, and so on, and the output from the
  final element is concatenated to the returned Chat object, which only
  contains the initial input and the final output, along with any
  previous exchanges. Since a ChatPipeline can be embedded into a Chat
  context, it is possible to nest pipelines, forming a tree.
}
\usage{
\method{c}{Agent}(...)
\method{c}{Chat}(...)
\method{c}{ChatPipeline}(...)
}
\arguments{
  \item{\dots}{
    Agent, Chat or ChatPipeline objects to combine. If an Agent, no
    context is preserved in the return value of \code{chat}, so every
    interaction happens is a transient, single exchange. If a Chat
    object, the context is preserved over calls to \code{chat}, so every
    interaction is recorded and can affect future iterations.
  }
}
\details{
  Programming agentic systems requires complex control flow. In a pure
  agentic system, the agents control the flow of computation, where
  agents call other agents, contextualizing requests as
  appropriate. Hybrid approaches, combining agentic and conventional
  logic, can be more appropriate, trading flexibility for
  predictability. In the general case, the conventional flow is probably
  best implemented directly as code, rather than through formalisms like
  sequential pipelines.

  However, formal structures like ChatPipeline provide a convenient
  abstraction, in that they can be invoked via \code{\link{chat}}, just
  like an individual agent. Another convenience is the management of
  context at each point in the pipeline.

  It is not clear whether formal chaining of agents and/or chat contexts
  will remain relevant as best practices evolve.  Thus, please regard
  this functionality as provisional and subject to change or even removal.
}
\note{Experimental and subject to change; see details.}
\value{
  ChatPipeline object, for use with \code{\link{chat}}.
}
\author{Michael Lawrence}
\examples{
\dontrun{
    pipeline <- c(
        adder = llama(server) |> instruct("Return a single number"),
        namer = llama(server) |> instruct("Return only the name of the number")
    )
    predict(pipeline, "1 + 1") # "Two"

    pipeline$namer <- chat(pipeline$namer) # now it preserves context
    cht <- chat(pipeline, "1 + 1")
    last_output(cht@model$namer) # which we can access
}
}
\keyword{ utilities }
