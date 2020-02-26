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
#' @importFrom vctrs new_rcrd
new_color_vctr <- function(vect = double(), text_color = NA, background = NA, style = NA){

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
    vctr = vect,
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
#' @param x vector to make into a color_vctr
#' @param text_color A vector of length 1 or same length as x. Details the
#'     color the text should be. Valid values can be found from the
#'     `valid_text_color()` function. NA means no text color.
#' @param background A vector of length 1 or same length as x. Details the
#'     background color of the text. Valid values can be found from the
#'     `valid_background()` function. NA means no background color.
#' @param style A vector of length 1 or same length as x. Details the
#'     style of the text Valid values can be found from the
#'     `valid_style()` function. NA means no styling.
#'
#' @return a color_vctr
#' @export

color_vctr <- function(x = double(), text_color = NA, background = NA, style = NA) {
    new_color_vctr(x, text_color, background, style)
}

#' Is the object a color_vctr?
#'s
#' Detect if object is a colortable vector
#'
#' @param x object to be checked if is a color_vctr
#' @export
is_color_vctr <- function(x){
  inherits(x,"color_vctr")
}


#####

# vctrs black magic lives here...not sure whats going on
# following https://vctrs.r-lib.org/articles/s3-vector.html

####

#' @export
#' @importFrom vctrs vec_ptype_abbr
vec_ptype_abbr.color_vctr <- function(x, ...) {
  "c_vctr"
}

#' @importFrom methods setOldClass
methods::setOldClass(c("color_vctr", "vctrs_vctr"))

#' @export
#' @importFrom vctrs vec_ptype2 vec_default_ptype2
vec_ptype2.color_vctr <- function(x, y, ...)
    UseMethod("vec_ptype2.color_vctr", y)

vec_ptype2.color_vctr.default <- function(x, y, ..., x_arg = "x", y_arg = "y")
    vec_default_ptype2(x, y, x_arg = x_arg, y_arg = y_arg)

vec_ptype2.color_vctr.color_vctr <- function(x, y, ...) new_color_vctr()

# Preserve all as color_vctr
vec_ptype2.color_vctr.character  <- function(x, y, ...) new_color_vctr()
vec_ptype2.color_vctr.double     <- function(x, y, ...) new_color_vctr()
vec_ptype2.color_vctr.integer    <- function(x, y, ...) new_color_vctr()
vec_ptype2.color_vctr.logical    <- function(x, y, ...) new_color_vctr()

vec_ptype2.character.color_vctr  <- function(x, y, ...) new_color_vctr()
vec_ptype2.double.color_vctr     <- function(x, y, ...) new_color_vctr()
vec_ptype2.integer.color_vctr    <- function(x, y, ...) new_color_vctr()
vec_ptype2.logical.color_vctr    <- function(x, y, ...) new_color_vctr()

#' @importFrom vctrs vec_cast
#' @export vec_cast.color_vctr
#' @importFrom vctrs field
vec_cast.color_vctr <- function(x, to, ...) UseMethod("vec_cast.color_vctr")
vec_cast.color_vctr.default <- function(x, to, ...) vec_default_cast(x, to)
vec_cast.color_vctr.color_vctr <- function(x, to, ...) x

# to color_vctrs
vec_cast.color_vctr.double <- function(x, to, ...) color_vctr(x)
vec_cast.color_vctr.integer <- function(x, to, ...) color_vctr(x)
vec_cast.color_vctr.character <- function(x, to, ...) color_vctr(x)
vec_cast.color_vctr.logical <- function(x, to, ...) color_vctr(x)

# from color_vctrs
vec_cast.double.color_vctr <- function(x, to, ...) field(x, "vctr")
vec_cast.integer.color_vctr <- function(x, to, ...) field(x, "vctr")
vec_cast.character.color_vctr <- function(x, to, ...) field(x, "vctr")
vec_cast.logical.color_vctr <- function(x, to, ...) field(x, "vctr")
vec_cast.list.color_vctr <- function(x, to, ...) lapply(seq_along(x),function(idx,x){x[idx]},x)
vec_cast.data.frame.color_vctr <- function(x, to, ...) data.frame(x)


