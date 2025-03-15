test_that("Langsmith templates work", {
    model <- llama(server) |> prompt_as("muhsinbashir/job-interview")
    expect_match(model@io@input@template, "interviewer")

    model <- llama(server) |> prompt_as("hardkothari/text_summary")
    expect_match(model@system_prompt_format@template, "summarizer")
    expect_match(model@io@input@template, "summary")
    
    model <- llama(server) |> prompt_as("vectrix/intent_detection")
    expect_match(model@io@input@template, "intent")
    ans <- model |> predict(list(chat_history = "", question = "Hello?"))
    expect_equal(ans$intent, "greeting")
})
