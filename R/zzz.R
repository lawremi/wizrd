## see: https://github.com/RConsortium/S7/issues/529
globalVariables("properties")

.onLoad <- function(...) {
    S7::methods_register()
}
