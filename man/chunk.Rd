\name{chunk}
\alias{chunk}
\alias{default_chunking}

\title{
  Text Chunking
}
\description{
  Chunks text for the purpose of generating embeddings and query
  approaches like RAG. Supported formats include: R character vectors,
  plain text files, Rd files, markdown files (including Rmd and Quarto),
  HTML files and PDF files.
}
\usage{
chunk(x, by = default_chunking(x))
default_chunking(x)
}
\arguments{
  \item{x}{
    A container or source of text to be chunked
  }
  \item{by}{
    A Chunking object representing the chunking strategy and its
    parameters, or a named list of such objects.  If \code{NULL}, no
    chunking is performed (the text is returned as a single chunk, if
    possible).
  }
}
\value{
  \code{chunk} returns a data.frame with a \dQuote{text} column
  containing the text chunks, along with a variable set of metadata
  columns, depending on the type of text and chunking strategy.

  \code{default_chunking} returns the default Chunking object to use for
  the given object. For a character vector or list (assumedly of
  filenames), it returns a list of default file extension to Chunking
  mappings (this registry will be exposed to extensions in the future).
}
\details{
  Here are the different types of input and how they are handled by
  default:
  \itemize{
    \item{string}{
      A vector of strings is chunked directly based on \code{by}. By
      default, \code{by} is a list, in which case the names of the
      vector are used to look up the appropriate Chunking object based
      on its name in the list. If there are no names, sentence-aligned
      word chunking is used.
    }
    \item{path}{
      If a file, reads it as a single string and chunks it according to
      \code{by}. If \code{by} is a list, the element whose name matches
      the file extension is used (see below for the built-in
      handlers). If there is no match, sentence-aligned chunking is
      used.
    }
    \item{md,Rmd,Qmd}{
      RMarkdown and Quarto files are first broken into
      sections. Optionally, each section can be further chunked based on
      the \code{@section_chunking} property. Typically these files are
      identified by their file extension as described above. It is also
      possible to pass an object from the \pkg{parsermd} package.
    }
    \item{HTML}{
      HTML files are converted to markdown using pandoc and then chunked
      as described above.
    }
    \item{PDF}{
      Blocks of text are extracted using \pkg{pdftools} and
      potentially further chunked according \code{@section_chunking}.
    }
    \item{\code{vignette()}}{
      The return value of \code{\link{vignette}}, a packageIQR object,
      is a convenient way to chunk all of the vignettes in a package.
    }
    \item{\code{Rd_db()}}{
      The return value of \code{\link{Rd_db}}, an Rd object, is a
      convenient way to chunk all of the man pages of a package.
    }
  }
  
  The common default sentence-aligned word chunking breaks a body of
  text into chunks with boundaries aligned to the starts/ends of
  sentences. It uses a default word limit (\code{@token_limit}) of 512
  and maximum word overlap (\code{max_overlap}) of 64.
    
  For more fine-grained control, the easiest approach currently is to
  call \code{default_chunking} on the input, take the desired Chunking
  object, modify its properties, and pass it via the \code{by}
  argument. In the future, more of the API will be exposed.
}
\seealso{
  \code{\link{text_store}} for storing and querying the chunks
}
\note{
  The text chunking capabilities \pkg{wizrd} are currently experimental
  and quite primitive. In particular, the token counting is just the
  word count, which will not be consistent with LLM tokenization; we
  hope to address this in the future. It mostly depends on whether
  anyone else finds text chunking useful.
}
\author{Michael Lawrence}
\examples{
\dontrun{
    chunks <- chunk(tools::Rd_db("S7"))
    text_store(nomic(), chunks)
    model <- llama() |> prompt_as(rag_from(store))
    predict(model, "new_property example")
}
}
\keyword{ utilities }
