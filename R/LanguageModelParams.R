LanguageModelParams <- new_class("LanguageModelParams",
                                 properties = list(
                                     temperature = nullable(prop_number_nn),
                                     top_p = nullable(prop_prob),
                                     top_k = nullable(prop_int_pos),
                                     max_tokens = nullable(prop_int_nn),
                                     presence_penalty = nullable(prop_number),
                                     frequency_penalty = nullable(prop_number),
                                     stop = new_property(
                                         NULL | class_character,
                                         validator = function(value) {
                                             if (anyNA(value))
                                                 "must not contain NAs"
                                         })
                                 ))
