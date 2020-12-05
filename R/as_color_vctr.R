#' Coerce object to a color_vctr
#'
#' Coercion function for generation of a color_vctr from an existing vector
#'
#' @rdname as_color_vctr
#'
#' @param x object to coerce
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
#' @export
as_color_vctr <- function(x, text_color = NA, background = NA, style = NA){
  UseMethod("as_color_vctr",x)
}

#' @rdname as_color_vctr
#' @export
as_color_vctr.default <- function(x, text_color = NA, background = NA, style = NA){
  stop(gettextf("cannot coerce class %s to a color_vctr",
                sQuote(deparse(class(x))[1L])), domain = NA)
}

#' @rdname as_color_vctr
#' @export
as_color_vctr.numeric <- function(x,text_color = NA, background = NA, style = NA){
  new_color_vctr(x,
                 text_color = text_color,
                 background = background,
                 style = style)
}

#' @rdname as_color_vctr
#' @export
as_color_vctr.color_vctr<- function(x,text_color = NA, background = NA, style = NA){
  x
}

#' @rdname as_color_vctr
#' @export
as_color_vctr.character <- as_color_vctr.numeric

#' @export
as_color_vctr.integer <- as_color_vctr.numeric

#' @rdname as_color_vctr
#' @export
as_color_vctr.logical <- as_color_vctr.numeric

