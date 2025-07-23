\name{chat}
\alias{chat}
\alias{predict.Agent}
\alias{predict.Chat}
\alias{chat.Agent}
\alias{chat.Chat}

\title{
  Interact with an Agent
}
\description{
  Interact with an agent, either by a single exchange (\code{predict})
  or by maintaining context over multiple exchanges (\code{chat}).
}

\section{Methods}{
  There are methods for both \code{predict} and \code{chat} on Chat and Agent
  objects.
  
  \subsection{Usage}{
    \describe{
      \item{Agent}{
	\itemize{
	  \item{\code{chat(x, input = NULL, stream_callback = NULL,
	      system_params = list(), env = parent.frame(), ...)}}
	  \item{\code{predict(object, input, env = parent.frame(), ...)}}
	}
      }
      \item{Chat}{
	\itemize{
	  \item{\code{chat(x, input = NULL, stream_callback = NULL, ...)}}
	  \item{\code{predict(object, input, ...)}}
	}
      }
    }
  }
  \subsection{Arguments}{
    \describe{
      \item{x,object}{
	An Agent or Chat object, which receives the input and
	generates a response.
      }
      \item{input}{
	An object that is sent to the model (appended to the context when
	\code{x} is a Chat object). Typically a string but could also be a
	\code{\link[=plot.raster]{raster}} object (for vision models) or any
	other R object supported by the extensible serialization
	mechanism. data.frames are converted to CSV, and other complex
	objects are serialized to JSON, by default.
      }
      \item{stream_callback}{
	A function that takes a single argument, a chunk of the model
	response, as a string. Typically only used in interactive chat
	settings, so that responses can be streamed to user as they are
	being generated.
      }
      \item{system_params}{
	When \code{x} is an Agent, a named list of strings used to
	instantiate the system prompt template according to
	\code{\link[glue]{glue}} semantics. See
	\code{\link{system_prompt_as}}. For advanced use only.
      }
      \item{env}{
	When \code{x} or \code{object} is an Agent, an environment
	that is used when resolving R symbols passed to tools. For advanced
	use only.
      }
      \item{\dots}{
	For \code{predict}, arguments passed to \code{chat}. For
	\code{chat}, arguments passed to the underlying backend.
      }
    }
  }
  \subsection{Value}{
    For \code{predict}, the model response. For \code{chat}, a Chat object.
  }
}
\author{Michael Lawrence}
\examples{
\dontrun{
    agent <- llama() |>
        instruct("Answer questions about the mtcars dataset:", mtcars)
    predict(agent, "What is the relationship between hp and fuel efficiency?")
    chat <- chat(agent,
                 "What is the relationship between hp and fuel efficiency?")
    chat <- chat(chat, "Can you suggest ways to visualize this relationship?")
    last_output(chat)
}
}
\keyword{ utilities }
\keyword{ methods }
