\name{mcp}
\alias{connect_mcp}
\alias{exec_mcp_server}
\alias{mcp_test_server}
\alias{tools}
\alias{resources}
\alias{prompts}
\alias{MCPSession}

\title{
  Model Context Protocol (MCP)
}
\description{
  Functions for interacting with MCP servers, which provide a standardized
  interface for language models to access tools and resources.
}
\usage{
connect_mcp(server)

exec_mcp_server(server, ...)

mcp_test_server(...)

\method{tools}{MCPSession}(x)

\method{resources}{MCPSession}(x)

\method{prompts}{MCPSession}(x)
}
\arguments{
  \item{server}{
    URL to the MCP server. For \code{exec_mcp_server}, this can be a
    command to start the server.
  }
  \item{\dots}{
    Arguments passed to the server process.
  }
  \item{x}{
    The MCP session returned by \code{connect_mcp}
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
    server <- exec_mcp_server("python -m wizrd.mcp")
    session <- connect_mcp(wizrd:::mcp_test_server())
    mcp_tools <- tools(session)
    result <- mcp_tools$add(1L, 2L)
}
}
\keyword{ utilities }
