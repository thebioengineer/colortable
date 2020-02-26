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
#' @importFrom vctrs new_vctr
new_color_vctr <- function(vect = double(), text_color = NA, background = NA, style = NA ){

  stopifnot(is.atomic(vect))

  stopifnot(length(text_color) == 1 | length(text_color) == length(vect))
  stopifnot(length(background) == 1 | length(background) == length(vect))
  stopifnot(length(style) == 1 | length(style) == length(vect))


  if (length(text_color) == 1)
    text_color <- rep(text_color, length(vect))
  if (length(background) == 1)
    background <- rep(background, length(vect))
  if (length(style) == 1)
    style <- rep(style, length(vect))

  new_rcrd(list(
    vect = vect,
    .text_color = text_color,
    .background = background,
    .style = style
  ),
  class = "color_vctr")
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

color_vctr <-
  function(x = double(),
           ...,
           text_color = NA,
           background = NA,
           style = NA) {
    UseMethod("color_vctr", x)
  }

#' @export
color_vctr.default <-
  function(x = double(),
           ...,
           text_color = NA,
           background = NA,
           style = NA) {
    new_color_vctr(
      c(x, ...),
      text_color = text_color,
      background = background,
      style = style
    )
  }

#' @export
color_vctr.color_vctr <- function(x = double(), ...) {
  coltable_nect_list <- list(x, ...)

  vect <- do.call('c', lapply(coltable_nect_list, function(z) {
    .subset(z, seq_along(z))
  }))

  text_color <-
    do.call('c', lapply(coltable_nect_list, attr, ".text_color"))
  background <-
    do.call('c', lapply(coltable_nect_list, attr, ".background"))
  style      <-
    do.call('c', lapply(coltable_nect_list, attr, ".style"))

  return(new_color_vctr(
    vect,
    text_color = text_color,
    background = background,
    style = style
  ))
}

#####

# vctrs black magic lives here...not sure whats going on
# following https://vctrs.r-lib.org/articles/s3-vector.html

####

#' @importFrom methods setOldClass
methods::setOldClass(c("color_vctr", "vctrs_vctr"))

#' @method vec_cast color_vctr
#' @export
#' @importFrom vctrs vec_ptype2
vec_ptype2.color_vctr <-
  function(x, y, ...)
    UseMethod("vec_ptype2.color_vctr", y)

#' @importFrom vctrs vec_default_ptype2
vec_ptype2.color_vctr.default <- function(x, y, ..., x_arg = "x", y_arg = "y") {
    vec_default_ptype2(x, y, x_arg = x_arg, y_arg = y_arg)
}
vec_ptype2.color_vctr.color_vctr <- function(x, y, ...) color_vctr()
vec_ptype2.color_vctr.character  <- function(x, y, ...) color_vctr()
vec_ptype2.color_vctr.double     <- function(x, y, ...) color_vctr()
vec_ptype2.color_vctr.integer    <- function(x, y, ...) color_vctr()
vec_ptype2.color_vctr.logical    <- function(x, y, ...) color_vctr()


#' @importFrom vctrs vec_cast
#' @method vec_cast color_vctr
#' @export vec_cast.color_vctr
vec_cast.color_vctr <- function(x, to, ...) UseMethod("vec_cast.color_vctr")

# to color_vctrs
vec_cast.color_vctr.default <- function(x, to, ...) vec_default_cast(x, to, ....)
vec_cast.color_vctr.color_vctr <- function(x, to, ...) color_vctr(x, ...)
vec_cast.color_vctr.double <- function(x, to, ...) color_vctr(x, ...)
vec_cast.color_vctr.integer <- function(x, to, ...) color_vctr(x, ...)
vec_cast.color_vctr.character <- function(x, to, ...) color_vctr(x, ...)

# from color_vctrs
vec_cast.double.color_vctr <- function(x, to, ...) vec_data(x, ...)
vec_cast.integer.color_vctr <- function(x, to, ...) vec_data(x, ...)
vec_cast.character.color_vctr <- function(x, to, ...) vec_data(x, ...)


