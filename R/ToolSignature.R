union_classes <- NULL | S7_class | S7_base_class | S7_union | S7_any |
    S7_S3_class | getClass("classRepresentation")

ToolSignature <- new_class("ToolSignature",
                           properties = list(
                               arguments = new_list_property(
                                   of = union_classes,
                                   named = TRUE
                               ),
                               value = union_classes
                           ))
