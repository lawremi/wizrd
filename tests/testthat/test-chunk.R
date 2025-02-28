test_that("Token splitting works", {
    # Simple text with no overlap
    text <- "This is the first sentence. This is the second sentence, which is longer. Here is the third. Finally, the fourth sentence."
    chunks <- chunk(text, token_limit = 10L, max_overlap = 0L)
    expected <- c(
        "This is the first sentence. ",
        "This is the second sentence, which is longer. ",
        "Here is the third. Finally, the fourth sentence." 
    )
    expect_identical(chunks$text, expected)
    
    # Simple text with overlap
    chunks <- chunk(text, token_limit = 13L, max_overlap = 8L)
    expected <- c(
        "This is the first sentence. This is the second sentence, which is longer. ",
        "This is the second sentence, which is longer. Here is the third. ",
        "Here is the third. Finally, the fourth sentence."
    )
    expect_identical(chunks$text, expected)
    
    # Small token limit
    chunks <- chunk(text, token_limit = 5L, max_overlap = 0L)
    expected <- c(
        "This is the first sentence. ",
        "This is the second sentence, which is longer. ",
        "Here is the third. ",
        "Finally, the fourth sentence." 
    )
    expect_identical(chunks$text, expected)
    
    # Test 4: Large token limit (entire text fits in one chunk)
    chunks <- chunk(text, token_limit = 50L, max_overlap = 0L)
    expected <- c(
        "This is the first sentence. This is the second sentence, which is longer. Here is the third. Finally, the fourth sentence."
    )
    expect_identical(chunks$text, expected)
    
    # Test 5: Single sentence
    text <- "Just one short sentence."
    chunks <- chunk(text, token_limit = 10L, max_overlap = 0L)
    expected <- c("Just one short sentence.")
    expect_identical(chunks$text, expected)
    
    # Test 6: Complex text with punctuation
    text <- "Dr. Smith is here! Isn't that great? Mr. Johnson, however, is late."
    chunks <- chunk(text, token_limit = 3L, max_overlap = 0L)
    expected <- c(
        "Dr. ",
        "Smith is here! ",
        "Isn't that great? ",
        "Mr. ",
        "Johnson, however, is late."
    )
    expect_identical(chunks$text, expected)
    
    # Test 7: Invalid inputs
    expect_error(chunk("Test sentence.", token_limit = -10, max_overlap = 5))
    
    expect_error(chunk("Test sentence.", token_limit = 10, max_overlap = 15))
})
