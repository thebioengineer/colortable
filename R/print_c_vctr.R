#' @title  print method for color_vctr
#' @param x object of colortable vector
#' @param ... print options to be passed on
#' @param method The output type to print to. Defaults to one of: "console","latex","html".
#' @export
print.color_vctr <- function(x, ..., quote = TRUE, method = print_method()){
  switch(method,
         "console" = print.color_vctr.console(x, ..., quote = quote),
         "latex" = print.color_vctr.latex(x, ..., quote = quote),
         "html" = print.color_vctr.html(x, ..., quote = quote),
         "gfm" = print.color_vctr.html(x, ..., quote = quote),
         stop("Method for ",print_method()," not implemented yet.")
  )
  invisible(x)
}

#' @importFrom cli cat_line
print.color_vctr.console <- function(x, ..., quote = quote){

  ## determine nrows to print
  x2 <- unclass(x)
  length_x2 <- length(x2)
  length_x2 <- ifelse(length_x2 > 1000, 1000, length_x2)
  format_width <- format.info(x2,...)[1] + ifelse(quote & is.character(x2), 2, 0)
  n_per_row <- floor( (options()$width - 5) / format_width + 1 ) - 2
  n_row <- ceiling(length_x2/n_per_row)

  print_vect <- do.call('c',lapply(seq_len(length_x2),function(idx){
          style2ansi(
            x2[idx],
            attr(x,".style")[idx],
            attr(x,".text_color")[idx],
            attr(x,".background")[idx],
            ..., quote = TRUE)}))

  output_vect <- vector("character", length = n_row)
  idx <- seq(1,length(x2),by = n_per_row)
  prefix <- formatC(paste0("[",idx,"]"),width = max(nchar(idx))+2 )
  for (i in seq_along(idx)) {
    idx_start <- idx[i]
    idx_end <- idx_start + n_per_row -1
    if(idx_end > length(x2)) {
      idx_end <- length(x2)
    }
    output_vect[i] <- paste(c(prefix[i],print_vect[idx_start:idx_end]),collapse = " ")
  }

  cat_line(output_vect)
}

#' @importFrom cli cat_line
print.color_vctr.html <- function(x,...){
  print_vect <- do.call('c',lapply(seq_along(x),function(idx){
          style2html(
            .subset(x,idx),
            attr(x,".style")[idx],
            attr(x,".text_color")[idx],
            attr(x,".background")[idx],
            ...)}))
  cat_line(paste(print_vect,collapse=" "),"\n")
}

#' @importFrom cli cat_line
print.color_vctr.latex <- function(x,...){
  print_vect <- do.call('c',lapply(seq_along(x),function(idx){
          style2tex(
            .subset(x,idx),
            attr(x,".style")[idx],
            attr(x,".text_color")[idx],
            attr(x,".background")[idx],
            ...)}))
  cat_line(paste(print_vect,collapse=" "),"\n")
}

#' @title  format method for color_vctror
#' @param x object of colortable cell
#' @export
format.color_vctr <- function(x, ..., method = print_method()){
  switch(method,
         "console" = format.color_vctr.console(x,...),
         "latex" = format.color_vctr.latex(x,...),
         "html" = format.color_vctr.html(x,...),
         "gfm" = format.color_vctr.html(x,...),
         stop("Method for ", print_method()," not implemented yet.")
  )
}

format.color_vctr.console <- function(x,...){
  x <- do.call('c',lapply(seq_along(x),function(idx){
    style2ansi(
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

print.color_vctr_output <- cat
