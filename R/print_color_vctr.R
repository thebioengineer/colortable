#' @title  print method for color_vctr
#' @param x object of colortable vector
#' @param ... print options to be passed on
#' @param method The output type to print to. Defaults to one of: "console","latex","html".
#' @export
print.color_vctr <- function(x, ..., method = print_method()){
  cat_line(format(x, ..., method = method))
  invisible(x)
}

#' @title  format method for color_vctror
#' @param x object of colortable cell
#' @param ... format options to be passed on
#' @param method The output type to print to. Defaults to one of: "console","latex","html".
#'
#' @export
format.color_vctr <- function(x, ..., method = print_method()){
  format_method <- switch(method,
         "console" = format.color_vctr.console,
         "latex" = format.color_vctr.latex,
         "html" = format.color_vctr.html,
         "gfm" = format.color_vctr.html,
         stop("Method for ", print_method()," not implemented yet.")
  )

  format_method(x, ..., method = method)
}

format.color_vctr.console <- function(x,...){
  x <- do.call('c',lapply(seq_along(x),function(idx){
    style2console(
      .subset(x,idx),
      attr(x,".style")[idx],
      attr(x,".text_color")[idx],
      attr(x,".background")[idx],
      ...)}))
  class(x) <- c("color_vctr_output","character")
  x
}

format.color_vctr.html <- function(x,...){
  x <- do.call('c',lapply(seq_along(x),function(idx){
    style2html(
      .subset(x,idx),
      attr(x,".style")[idx],
      attr(x,".text_color")[idx],
      attr(x,".background")[idx],
      ...)}))
  class(x) <- c("color_vctr_output","character")
  x
}

format.color_vctr.latex <- function(x,...){
  x <- do.call('c',lapply(seq_along(x),function(idx){
    style2tex(
      .subset(x,idx),
      attr(x,".style")[idx],
      attr(x,".text_color")[idx],
      attr(x,".background")[idx],
      ...)}))
  class(x) <- c("color_vctr_output","character")
  x
}
