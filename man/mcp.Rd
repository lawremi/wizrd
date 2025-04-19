\name{mcp}
\alias{mcp_connect}
\alias{tools}
\alias{resources}
\alias{prompts}

\title{
  Model Context Protocol Support
}
\description{
  Execute and/or connect to a Model Context Protocol (MCP) server and
  retrieve tools, resources or prompts for use with models.
}
\usage{
mcp_exec_server(command, args = list())
mcp_connect(server)
tools(x)
resources(x)
prompts(x)
}
\arguments{
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
  \item{server}{
    A specification of an MCP server. For local servers, this is
    typically a commandline. For remote servers, it should be a
    websocket (ws://) URL.
  }
  \item{x}{
    The MCP session returned by \code{mcp_connect}
  }
}
\details{
  MCP is an emerging standard for centrally providing tools, resources
  and prompts to different clients that interface with
  LLMs. The \pkg{wizrd} package is one such client.

  The \code{tools}, \code{resources} and \code{prompts} functions each
  return a list of functions for different purposes.
  \itemize{
    \item{The return value of \code{tools} can be passed directly to
      \code{\link{equip}} to enable a model to make use of the tools
      provided by the MCP server.
    }
    \item{Calling a function returned by \code{resources} will return a
      character vector, which might be text or a base64-encoded image or
      audio stream. A typical use would be to pass them to a model,
      potentially by \code{\link{chunking}}ing them and indexing them for
      use in RAG.
    }
    \item{The functions returned by \code{prompts} can be called to
      return a list of chat message objects, representing an initial
      context for a chat. The list can be passed to \code{\link{chat}}
      or \code{\link{predict}}, potentially after appending it with
      additional user input. Function arguments with a default value
      of \code{NULL} are optional.
    }
  }
}
\value{
  \code{mcp_exec_server} returns a \link[processx]{process}
  object. \code{mcp_connect} returns an MCP session
  object. \code{tools}, \code{resources} and \code{prompts} each return
  a list of functions. In the case of \code{tools}, the functions are
  \code{\link[=tool]{Tool}} objects.
}
\author{Michael Lawrence}
\examples{
\dontrun{
    session <- mcp_connect(wizrd:::mcp_test_server())
    model <- llama()

    mcp_tools <- tools(session)
    result <- mcp_tools$add(1L, 2L) # call as an ordinary function
    model |> equip(mcp_tools) |> # or provide as tool to model
        predict("use the add tool to add 1 and 2")

    r <- resources(session)
    r$get_greeting("R")

    pf <- prompts(session)
    ## register as function template via prompt_as()
    model |> prompt_as(pf$ask_review) |>
        predict(list(code_snippet = "for (i in 1:length(n)) v <- c(v, i)",
                     language = "R"))
    ## or call function to generate initial chat context
    error_message <-
        "Error in if (x) y else x (from #1) : argument is of length zero"
    msgs <- pf$debug_session_start(error_message)
    info <- "Trying to use if() in R. Traceback: 1: fun(integer())"
    predict(model, c(msgs, info))
}
}
\keyword{ utilities }
