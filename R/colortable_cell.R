
#' @exportClass colortable_vect

new_cell <- function(x, text_color = NA, background = NA, style = NA){
  stopifnot(length(x) == 1)
  structure(
    .Data = x,
    .text_color = text_color,
    .background = background,
    .style = style,
    class = c("colortable_cell")
  )
}

#' @title Is a colortable_cell
#' @description detect if object is a colortable cell
#' @export
is_colortable_cell <- function(x){
  inherits(x,"colortable_cell")
}


#' @export
as_colortable_cell <- function(x,text_color = NA, background = NA, style = NA){
  as_colortable_vect(
          x,
          text_color = text_color,
          background = background,
          style = style)
}

#' @export
colortable_cell <- function(x, text_color = NA, background = NA, style = NA){
  as_colortable_cell(x,
                     text_color = text_color,
                     background = background,
                     style = style)
}

as.list.colortable_cell <- function(x,...){
  list(x)
}
