#' @title  print method for colortable_vect
#' @param x object of colortable vector
#' @param ... print options to be passed on
#' @param method The output type to print to. Defaults to one of: "console","latex","html".
#' @export
print.colortable_vect <- function(x, ..., method = print_method()){
  switch(method,
         "console" = print.colortable_vect.console(x,...),
         "latex" = print.colortable_vect.latex(x,...),
         "html" = print.colortable_vect.html(x,...),
         "gfm = print.colortable_vect.html(x,...)," = print.colortable_vect.html(x,...),
         stop("Method for ",print_method()," not implemented yet.")
  )
  invisible(x)
}

print.colortable_vect.console <- function(x,...){
  print_vect <- do.call('c',lapply(seq_along(x),function(idx){
          style2ansi(
            .subset(x,idx),
            attr(x,".style")[idx],
            attr(x,".text_color")[idx],
            attr(x,".background")[idx],
            ...)}))

  cat(paste(print_vect,collapse=" "),"\n")
}

print.colortable_vect.html <- function(x,...){
  print_vect <- do.call('c',lapply(seq_along(x),function(idx){
          style2html(
            .subset(x,idx),
            attr(x,".style")[idx],
            attr(x,".text_color")[idx],
            attr(x,".background")[idx],
            ...)}))
  cat(paste(print_vect,collapse=" "),"\n")
}

print.colortable_vect.latex <- function(x,...){
  print_vect <- do.call('c',lapply(seq_along(x),function(idx){
          style2tex(
            .subset(x,idx),
            attr(x,".style")[idx],
            attr(x,".text_color")[idx],
            attr(x,".background")[idx],
            ...)}))
  cat(paste(print_vect,collapse=" "),"\n")
}

#' @title  format method for colortable_vector
#' @param x object of colortable cell
#' @export
format.colortable_vect <- function(x, ..., method = print_method()){
  switch(method,
         "console" = format.colortable_vect.console(x,...),
         "latex" = format.colortable_vect.latex(x,...),
         "html" = format.colortable_vect.html(x,...),
         "gfm" = format.colortable_vect.html(x,...),
         stop("Method for ", print_method()," not implemented yet.")
  )
}

format.colortable_vect.console <- function(x,...){
  x <- do.call('c',lapply(seq_along(x),function(idx){
    style2ansi(
      .subset(x,idx),
      attr(x,".style")[idx],
      attr(x,".text_color")[idx],
      attr(x,".background")[idx],
      ...)}))
  class(x) <- c("colortable_vect_output","character")
  x
}

format.colortable_vect.html <- function(x,...){
  x <- do.call('c',lapply(seq_along(x),function(idx){
    style2html(
      .subset(x,idx),
      attr(x,".style")[idx],
      attr(x,".text_color")[idx],
      attr(x,".background")[idx],
      ...)}))
  class(x) <- c("colortable_vect_output","character")
  x
}

format.colortable_vect.latex <- function(x,...){
  x <- do.call('c',lapply(seq_along(x),function(idx){
    style2tex(
      .subset(x,idx),
      attr(x,".style")[idx],
      attr(x,".text_color")[idx],
      attr(x,".background")[idx],
      ...)}))
  class(x) <- c("colortable_vect_output","character")
  x
}
