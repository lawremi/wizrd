FormatBinding <- new_class("FormatBinding",
                           properties = list(
                               input = SerialFormat,
                               output = SerialFormat
                           ))

method(describe, list(FormatBinding, LanguageModel)) <- function(x, to) {
    paste(c(input_instructions(x@input, to), output_instructions(x@output, to)),
          collapse = "\n\n")
}
