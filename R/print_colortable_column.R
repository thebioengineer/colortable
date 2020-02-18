
#' @export
print.colortable_column <- function(x, ..., method = print_method()){
    switch(method,
           "console" = print.colortable_column.console(x,...),
           "latex" = print.colortable_column.latex(x,...),
           "html" = print.colortable_column.html(x,...),
           stop("Method for",print_method()," not implemented yet.")
    )
    invisible(x)
}

print.colortable_column.console <- function(x,...){
    print_vect <- do.call('c',lapply(seq_along(x[[1]]),function(idx){
      style2ansi(
        .subset(x[[1]],idx),
        attr(x,".style")[idx],
        attr(x,".text_color")[idx],
        attr(x,".background")[idx],
        ...)
      }))
    cat(print_vect)
    invisible(data.frame(print_vect))
}

print.colortable_column.html <- function(x,...){
    print_vect <- do.call('c',lapply(seq_along(x),function(idx){
      style2html(
        .subset(x,idx),
        attr(x,".style")[idx],
        attr(x,".text_color")[idx],
        attr(x,".background")[idx],
        ...)}))
    data.frame(print_vect)
}

print.colortable_column.latex <- function(x,...){
    print_vect <- do.call('c',lapply(seq_along(x),function(idx){
      style2tex(
        .subset(x,idx),
        attr(x,".style")[idx],
        attr(x,".text_color")[idx],
        attr(x,".background")[idx],
        ...)}))
    data.frame(print_vect)
}
