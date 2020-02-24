#' @export
as_color_vctr <- function(x, text_color = NA, background = NA, style = NA){
  UseMethod("as_color_vctr",x)
}

#' @export
as_color_vctr.default <- function(x, text_color = NA, background = NA, style = NA){
  stop(gettextf("cannot coerce class %s to a color_vctr",
                sQuote(deparse(class(x))[1L])), domain = NA)
}

#' @export
as_color_vctr.numeric <- function(x,text_color = NA, background = NA, style = NA){
  new_color_vctr(x,
                 text_color = text_color,
                 background = background,
                 style = style)
}

#' @export
as_color_vctr.color_vctr<- function(x,text_color = NA, background = NA, style = NA){
  x
}

#' @export
as_color_vctr.character <- as_color_vctr.numeric

#' @export
as_color_vctr.integer <- as_color_vctr.numeric

#' @export
as_color_vctr.logical <- as_color_vctr.numeric

