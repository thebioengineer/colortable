#' @title  print method for colortable_cells
#' @param x object of colortable cell
#' @export
print.colortable_cell <- function(x,..., method = print_method()){
  switch(method,
         "console" = print.colortable_cell.console(x,...),
         "latex" = print.colortable_cell.latex(x,...),
         "html" = print.colortable_cell.html(x,...),
         "gfm" = format.colortable_cell.html(x,...),
         stop("Method for",print_method()," not implimented yet.")
  )
  invisible(x)
}

print.colortable_cell.console <- function(x,...){
  cat(style2ansi(x,attr(x,".style"), attr(x,".text_color"),attr(x,".background"),...),"\n")
}

print.colortable_cell.html <- function(x,...){
  cat(style2html(x,attr(x,".style"), attr(x,".text_color"),attr(x,".background"),...),"\n")
}

print.colortable_cell.latex <- function(x,...){
  cat(style2tex(x,attr(x,".style"), attr(x,".text_color"),attr(x,".background"),...),"\n")
}

#' @title  format method for colortable_cells
#' @param x object of colortable cell
#' @export
format.colortable_cell <- function(x,..., method = print_method()){
  switch(method,
         "console" = format.colortable_cell.console(x,...),
         "latex" = format.colortable_cell.latex(x,...),
         "html" = format.colortable_cell.html(x,...),
         "gfm" = format.colortable_cell.html(x,...),
         stop("Method for",print_method()," not implimented yet.")
  )
}

format.colortable_cell.console <- function(x,...){
  x <- style2ansi(x,...)
  class(x) <- c("colortable_cell_output")
}

format.colortable_cell.html <- function(x,...){
  x <- style2html(x,...)
  class(x) <- c("colortable_cell_output")
  x
}

format.colortable_cell.latex <- function(x,...){
  x <- style2tex(x,...)
  class(x) <- c("colortable_cell_output")
  x
}
