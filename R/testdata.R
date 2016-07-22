#' Title
#'
#' @param type the type of the institution.
#'
#' @return Subset of the data.
#' @export

testdata <- function(type) {
  for (i in 1:length(naf)) {
    naf[i] <- stringi::stri_unescape_unicode(naf[[i]])
  }
  set <- naf[naf$kind==type, ]

  return(set)
}
