#' Create a color_vctr from source data
#'
#' This is a method to create the a new color_vctr class
#'
#' @param vect The input vector to convert to a color_vctr. Must be an atomic.
#' @param text_color A vector of length 1 or same length as vect. Details the
#'     color the text should be. Valid values can be found from the
#'     `valid_text_color()` function.NA means no text color.
#' @param background A vector of length 1 or same length as vect. Details the
#'     background color of the text. Valid values can be found from the
#'     `valid_background()` function. NA means no background color.
#' @param style A vector of length 1 or same length as vect. Details the
#'     style of the text Valid values can be found from the
#'     `valid_style()` function. NA means no styling.
#'
#' @exportClass color_vctr
new_color_vctr <- function(vect, text_color = NA, background = NA, style = NA ){

  stopifnot(is.atomic(vect))
  stopifnot(length(text_color) == 1 | length(text_color) == length(vect))
  stopifnot(length(background) == 1 | length(background) == length(vect))
  stopifnot(length(style) == 1 | length(style) == length(vect))


  if(length(text_color) == 1)
    text_color <- rep(text_color,length(vect))
  if(length(background) == 1)
    background <- rep(background,length(vect))
  if(length(style) == 1)
    style <- rep(style,length(vect))

  return(
    structure(
      vect,
      ".text_color" = text_color,
      ".background" = background,
      ".style" = style,
      class = "color_vctr"
    )
  )
}


#' Create a color_vctr
#'
#' This is the generic method method dispatches color_vctr generation based on
#' the first argument.
#'
#' @param x data source determining method dispatch
#' @param ... additional elements to generate the color_vctr
#' @param text_color A vector of length 1 or same length as vect. Details the
#'     color the text should be. Valid values can be found from the
#'     `valid_text_color()` function.NA means no text color.
#' @param background A vector of length 1 or same length as vect. Details the
#'     background color of the text. Valid values can be found from the
#'     `valid_background()` function. NA means no background color.
#' @param style A vector of length 1 or same length as vect. Details the
#'     style of the text Valid values can be found from the
#'     `valid_style()` function. NA means no styling.
#'
#' @return a color_vctr
#' @export

color_vctr <- function(x, ..., text_color = NA, background = NA, style = NA){
  UseMethod("color_vctr",x)
}

#' @export
color_vctr.default <- function(x,...,text_color = NA, background = NA, style = NA) {
  new_color_vctr(
    c(x, ...),
    text_color = text_color,
    background = background,
    style = style
    )
}

#' @export
color_vctr.color_vctr <- function(x,...){

  coltable_nect_list <- list(x,...)

  vect <- do.call('c', lapply(coltable_nect_list, function(z) {
      .subset(z, seq_along(z))
    }))

  text_color <- do.call('c', lapply(coltable_nect_list, attr, ".text_color"))
  background <- do.call('c', lapply(coltable_nect_list, attr, ".background"))
  style      <- do.call('c', lapply(coltable_nect_list, attr, ".style"))

  return(new_color_vctr(
    vect,
    text_color = text_color,
    background = background,
    style = style
  ))
}
