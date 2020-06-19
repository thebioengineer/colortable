.onLoad <- function(libname, pkgname) {
  options("colortable.precidence" = "left")
  options("colortable.color_approx.method" = "euclidian")

  if (knitr::is_latex_output()) {
    rmarkdown::latex_dependency("xcolor")#, extra_lines = color_key_latex$code)
    rmarkdown::latex_dependency("ulem")
  }

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


