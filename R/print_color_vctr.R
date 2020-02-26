
#' @title  format method for color_vctror
#' @param x object of colortable cell
#' @param ... format options to be passed on
#' @param method The output type to print to. Defaults to one of: "console","latex","html".
#' @aliases format.color_vctr.console format.color_vctr.html format.color_vctr.latex
#' @usage format.color_vctr.console format.color_vctr.html format.color_vctr.latex
#' @name format.color_vctr
#' @export
format.color_vctr <- function(x, ..., method = print_method()){
  format_method <- switch(method,
         "console" = format.color_vctr.console,
         "latex" = format.color_vctr.latex,
         "html" = format.color_vctr.html,
         "gfm" = format.color_vctr.html,
         stop("Method for ", print_method()," not implemented yet.")
  )
  if (length(x) > 0) {
    format_method(x, ..., method = method)
  } else{
    "<color_vctr[0]>"
  }
}

#' format color_vctr for printing to console
#' @rdname format.color_vctr
#' @importFrom vctrs field
#' @param x color_vctr to be printed
#' @param ... additional settings to be passed to format
format.color_vctr.console <- function(x,...){
  x <- style2consoleV(
    field(x, "vctr"),
    field(x, ".style"),
    field(x, ".text_color"),
    field(x, ".background"),
    ...
  )
  class(x) <- c("color_vctr_output", "character")
  x
}

#' format color_vctr for printing to html
#' @rdname format.color_vctr
#' @importFrom vctrs field
#' @param x color_vctr to be printed
#' @param ... additional settings to be passed to format
format.color_vctr.html <- function(x,...){
  x <- style2htmlV(
    vctrs::field(x, "vctr"),
    vctrs::field(x, ".style"),
    vctrs::field(x, ".text_color"),
    vctrs::field(x, ".background"),
    ...
  )
  class(x) <- c("color_vctr_output", "character")
  x
}

#' format color_vctr for printing to latex
#' @rdname format.color_vctr
#' @importFrom vctrs field
#' @param x color_vctr to be printed
#' @param ... additional settings to be passed to format
format.color_vctr.latex <- function(x,...){
  x <- style2texV(
    vctrs::field(x, "vctr"),
    vctrs::field(x, ".style"),
    vctrs::field(x, ".text_color"),
    vctrs::field(x, ".background"),
    ...
  )
  class(x) <- c("color_vctr_output","character")
  x
}
