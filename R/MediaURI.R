MediaURI <- new_class("MediaURI", class_character,
                      properties = list(
                          type = prop_string,
                          category = new_string_property(
                              getter = \(self) {
                                  sub("/.*", "", self@type)
                              }
                          )
                      ))

validate_MediaURI <- function(self) {
    category <- self@category
    class_prefix <- tolower(sub(".*?::", "", sub("URI$", "", class(self)[1L])))
    if (category != class_prefix)
        paste0("@category must be '", class_prefix, "', ",
               "not '", category, "'")
}

ImageURI <- new_class("ImageURI", MediaURI, validator = validate_MediaURI)

AudioURI <- new_class("ImageURI", MediaURI, validator = validate_MediaURI)

method(convert, list(union_raster, MediaURI)) <- function(from, to) {
    require_ns(c("base64enc", "png"), "encode images")
    
    if (inherits(from, "nativeRaster")) { # like from dev.capture() or readPNG()
        image <- from
    } else {
        m <- as.matrix(as.raster(from))
        rgb <- col2rgb(m, alpha = TRUE) / 255L
        image <- array(t(rgb), c(dim(m), 4L))
    }
    
    uri <- base64enc::dataURI(png::writePNG(image), mime = "image/png")

    ImageURI(uri, type = "image/png")
}

data_uri <- function(data, encoding = "base64", mimeType = "") {
    paste0("data:", mimeType, ";", encoding, ",", data)
}
