\name{mcp}
\alias{connect_mcp}
\alias{start_mcp}
\alias{tools}
\alias{resources}
\alias{prompts}
\alias{MCPSession}

\title{
  Model Context Protocol (MCP)
}
\description{
  Functions for interacting with MCP servers, which provide a standardized
  interface for agents to access tools, resources and prompts.
}
\usage{
connect_mcp(server)

start_mcp(command, args = list())

tools(x, ...)

resources(x, ...)

prompts(x, ...)
}
\arguments{
  \item{server}{
    A specification of an MCP server. For local servers, this is
    typically a commandline. For remote servers, it should be a
    URL, typically just a hostname.
  }
  \item{command}{
    Name of command to execute. If this is \dQuote{uvx},
    and \command{uvx} is not found on the path, \pkg{reticulate} is used to
    install \command{uvx} and run the command (\command{uvx} is a
    popular Python package manager, so this ensures that a registered
    Python MCP server can be automatically installed anywhere).
  }
  \item{args}{
    List of string arguments to pass to \code{command}
  }
  \item{x}{
    The MCP session returned by \code{connect_mcp}
  }
  \item{\dots}{
    Additional arguments for methods.
  }
}
\value{
  \code{connect_mcp} returns an MCP session object. \code{tools},
  \code{resources}, and \code{prompts} return lists of functions that can
  be called to interact with the server.
}
\author{Michael Lawrence}
\examples{
\dontrun{
    session <- connect_mcp(wizrd:::start_test_mcp())
    mcp_tools <- tools(session)
    result <- mcp_tools$add(1L, 2L)
}
}
\keyword{ utilities }
