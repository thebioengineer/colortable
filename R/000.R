.onLoad <- function(libname, pkgname) {
  options("colortable.precidence" = "left")
}


#' Merge semantics for colortable vector arithmetic
#'
#' Set how tables with different styling should merge,
#' by deffering to left styling when it exists, right styling,
#' or attempt to blend them together.
#'
#' @param precedence one of three options: left, right or blended
#'
#' @export
set_color_vctr_precedence <- function(precedence = c("left","right","blended","mixed")){
  precedence <- match.arg(precedence)
  options("colortable.precidence" = precedence)
}
