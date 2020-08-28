#' Is the object a color_vctr?
#'
#' Detect if object is a colortable vector
#'
#' @param x object to be checked if is a color_vctr
#' @export
#'
is_color_vctr <- function(x){
  inherits(x,"color_vctr")
}
