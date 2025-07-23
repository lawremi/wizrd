# wizrd: An R Interface to Large Language Models

## Overview

The wizrd package enables embedding Large Language Models (LLMs) in R programs by
treating them as functions with structured inputs and outputs. It supports local
and remote LLMs, including Ollama, llama.cpp, OpenAI, and Azure, via an
extensible abstraction. Features include prompt templating, tool calling, and
experimental support for text embedding, vector stores, and Results Augmented
Generation (RAG).

## Installation

```r
install.packages("remotes")
remotes::install_github("lawremi/wizrd")
```

## Quick Start

```r
library(wizrd)

# Use a local LLM (requires Ollama)
agent <- llama()
predict(agent, "Describe the mtcars dataset")
```

For advanced usage, including parameterized prompts, tool calling, structured
output, and retrieval-augmented generation (RAG), see the package vignette:

```r
vignette("wizrd")
```

## Documentation and Support

- Reference: `help(package = "wizrd")`
- Website: [https://lawremi.github.io/wizrd/](https://lawremi.github.io/wizrd/)
- GitHub Issues: [https://github.com/lawremi/wizrd/issues](https://github.com/lawremi/wizrd/issues) 