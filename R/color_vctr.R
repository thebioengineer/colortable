#' Create a color_vctr from source data
#'
#' This is a method to create the a new color_vctr class
#'
#' @param vect The input vector to convert to a color_vctr. Must be an atomic.
#' @param text_color A vector of length 1 or same length as vect. Details the
#'     color the text should be. Valid values can be found from the
#'     `valid_colors()` function.NA means no text color.
#' @param background A vector of length 1 or same length as vect. Details the
#'     background color of the text. Valid values can be found from the
#'     `valid_colors()` function. NA means no background color.
#' @param style A vector of length 1 or same length as vect. Details the
#'     style of the text Valid values can be found from the
#'     `valid_style()` function. NA means no styling.
#'
#' @exportClass color_vctr
#' @import vctrs
#' @importFrom vctrs vec_assert new_rcrd
#' @examples
#'
#' color_vctr(1:5, text_color = "blue", background = "yellow", style = "bold")
#' color_vctr(LETTERS, text_color = color_scale("Blues"), background = "darkgrey", style = "italic")

new_color_vctr <- function(vect = double(), text_color = NA_character_, background = NA_character_, style = NA_character_){

  # assert vect is an atomic
  vec_assert(vect, atomic(vect))

  if (is.function(text_color))
    text_color <- text_color(vect)
  if (is.function(background))
    background <- background(vect)

  text_color <- vec_assert_style(text_color, size = length(vect))
  background <- vec_assert_style(background, size = length(vect))
  style      <- vec_assert_style(style     , size = length(vect))

  new_rcrd(
    list(
      vctr = vect,
      .text_color = text_color,
      .background = background,
      .style = style
    ),
    class = "color_vctr"
  )
}

#' @importFrom vctrs vec_assert
vec_assert_style <- function(x, size){
  arg <- match.call()$x
  ptype <-
  if (all(is.na(x))) {
    ptype <- logical()
    x <- rep(NA_character_, times = length(x))
  }
  if(length(x) == 1){
    x <- rep(x, size)
  }
  vec_assert(x,ptype = character(), size = size, arg = arg)
  return(x)
}

atomic <- function(x){
  type <- class(x[1])
  if (type %in% c("logical",
                  "integer",
                  "numeric",
                  "double",
                  "complex",
                  "character",
                  "raw",
                  "Date")) {
    return(x[0])
  }else if (type %in% "factor"){
    vctrs::new_factor(levels = levels(x))
  }else{
    character()
  }
}

#' Create a color_vctr
#'
#' This is the generic method method dispatches color_vctr generation based on
#' the first argument.
#'
#' @param x data source determining method dispatch
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

color_vctr <- function(x = double(), text_color = NA, background = NA, style = NA) {
  new_color_vctr(x, text_color, background, style)
}

#' @importFrom vctrs field vec_ptype_abbr
color_vctr_class <- function(x) vec_ptype_abbr(field(x,"vctr"))


#' Is the object a color_vctr?
#'
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
  paste0("c_vctr<",color_vctr_class(x),">")
}

#' @export
#' @importFrom vctrs vec_ptype_full
vec_ptype_full.color_vctr <- function(x, ...) {
  paste0("color_vctr<",color_vctr_class(x),">")
}

#' @importFrom methods setOldClass
methods::setOldClass(c("color_vctr", "vctrs_vctr"))


#' @export
#' @method vec_proxy_equal color_vctr
#' @importFrom vctrs field vec_proxy_equal
vec_proxy_equal.color_vctr <- function(x){
  field(x,"vctr")
}
#' @export
#' @method vec_proxy_compare color_vctr
#' @importFrom vctrs field vec_proxy_compare
vec_proxy_compare.color_vctr <- function(x, ...) {
  field(x,"vctr")
}

#' @export
#' @method vec_math color_vctr
#' @importFrom vctrs field vec_math
vec_math.color_vctr <- function(.fn, .x, ...) {
  vec_math_base(.fn, field(.x,"vctr"), ...)
}

#' Arithmatic
#' @export
#' @method vec_arith color_vctr
#' @importFrom vctrs field vec_arith
vec_arith.color_vctr <- function(op, x, y, ...) {
  UseMethod("vec_arith.color_vctr", y)
}
#' @importFrom vctrs stop_incompatible_op
vec_arith.color_vctr.default <- function(op, x, y, ...) {
  stop_incompatible_op(op, x, y)
}


#' @export
#' @method vec_arith.color_vctr MISSING
#' @importFrom vctrs field
vec_arith.color_vctr.MISSING <- function(op, x, y, ...) {
  switch(op,
         `-` = new_color_vctr(field(x,"vctr") * -1, field(x,".text_color"), field(x,".background"), field(x,".style")),
         `+` = x,
         stop_incompatible_op(op, x, y)
  )
}
