## Testing notes:

## - We can test basic generation using the embedded stories260K model,
##   which we can convert into a llama file and access via HTTP.
## - Tool use will require a fine tuned model to run locally, perhaps:
##   - https://huggingface.co/Groq/Llama-3-Groq-8B-Tool-Use/tree/main
##   - https://github.com/unclecode/callama
##   - or just Llama 3.1 8B
##   Tests will likely need to be disabled on CRAN, unless we can get
##   something like the 1.13 GB TinyLlama one working. Unfortunately
##   the Llama.cpp server embedded within a llamafile does not support
##   function calling, so we will have to use ollama.
## - To formally constrain to JSON schema (at the grammar level), we
##   would need to use the Llama.cpp server (so a llamafile). The only
##   way to get function calling and constrained output at the same
##   time is the python llama-cpp-server, which is harder to install
##   and run.

test_that("predict() works", {
    server <- start_llama_cpp_server()
    model <- llm(server)

    expect_string(predict(model, "Where did R come from?"))
})
