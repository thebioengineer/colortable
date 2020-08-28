.onLoad <- function(libname, pkgname) {
  options("colortable.precidence" = "left")
  options("colortable.color_approx.method" = "euclidian")

  colortable_shims()

  if (knitr::is_latex_output()) {
    rmarkdown::latex_dependency("xcolor")#, extra_lines = color_key_latex$code)
    rmarkdown::latex_dependency("ulem")
  }

}





