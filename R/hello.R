#' This is the Title.
#'
#' This is the description.
#'
#' These are further details. What's going on here?
#' Text accompanying the custom section.
#'
#' @param x A description of the parameter 'x'. The
#'   description can span multiple lines.
#'
#' @export


hello <- function() {
  txt <- stringi::stri_unescape_unicode('\u4e16\u754c\uff0c\u4f60\u597d\uff01')
  print(txt)
}
