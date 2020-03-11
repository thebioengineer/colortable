#' @title  print method for color_vctr
#' @param x object of colortable vector
#' @param ... print options to be passed on
#' @param method The output type to print to. Defaults to one of: "console","latex","html".
#' @export
print.color_vctr <- function(x, ..., method = print_method()){
  formatted_x <- format(x, ..., method = method)
  cat_line(format_console_vctr_print(x, formatted_x, ...))
  invisible(x)
}


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
  style2consoleV(
    field(x, "vctr"),
    field(x, ".style"),
    field(x, ".text_color"),
    field(x, ".background"),
    ...
  )
}

#' format color_vctr for printing to html
#' @rdname format.color_vctr
#' @importFrom vctrs field
#' @param x color_vctr to be printed
#' @param ... additional settings to be passed to format
format.color_vctr.html <- function(x,...){
  style2htmlV(
    vctrs::field(x, "vctr"),
    vctrs::field(x, ".style"),
    vctrs::field(x, ".text_color"),
    vctrs::field(x, ".background"),
    ...
  )
}

#' format color_vctr for printing to latex
#' @rdname format.color_vctr
#' @importFrom vctrs field
#' @param x color_vctr to be printed
#' @param ... additional settings to be passed to format
format.color_vctr.latex <- function(x,...){
  style2texV(
    vctrs::field(x, "vctr"),
    vctrs::field(x, ".style"),
    vctrs::field(x, ".text_color"),
    vctrs::field(x, ".background"),
    ...
  )
}

#' format coolor_vctr vector printing to console
#' @rdname format.color_vctr
#' @importFrom vctrs field
#' @param x color_vctr to be prinited
#' @param formatted_x formatted color_vctr for printing
format_console_vctr_print <- function(x,formatted_x,...){

  x2 <- field(x,"vctr")
  length_x2 <- length(x2)
  length_x2 <- ifelse(length_x2 > 1000, 1000, length_x2)
  format_info <- format.info(x2,...)
  n_per_row <- floor( (options()$width - 5) / format_info[1] + 1 ) - 2
  n_row <- ceiling(length_x2/n_per_row)

  output_vect <- vector("character", length = n_row)
  idx <- seq(1,length(x2),by = n_per_row)
  prefix <- formatC(paste0("[",idx,"]"),width = max(nchar(idx))+2 )
  for (i in seq_along(idx)) {
    idx_start <- idx[i]
    idx_end <- idx_start + n_per_row -1
    if(idx_end > length(x2)) {
      idx_end <- length(x2)
    }
    output_vect[i] <- paste(c(prefix[i],formatted_x[idx_start:idx_end]),collapse = " ")
  }

  output_vect
}

