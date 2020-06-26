
#' @export
vec_cast.color_vctr.color_vctr <- function(x, to, ...) {
  coerce_color_vctr(x, to)
}

#' @export
vec_cast.color_vctr.double <- function(x, to, ...) coerce_to_color_vctr(x, to)
#' @export
#' @method vec_cast.double color_vctr
vec_cast.double.color_vctr <- function(x, to, ...) coerce_color_vctr(x, to)

#' @export
vec_cast.color_vctr.integer <- function(x, to, ...)  coerce_to_color_vctr(x, to)
#' @export
#' @method vec_cast.integer color_vctr
vec_cast.integer.color_vctr <- function(x, to, ...) coerce_color_vctr(x, to)

#' @export
vec_cast.color_vctr.integer64 <- function(x, to, ...)  coerce_to_color_vctr(x, to)
#' @export
#' @method vec_cast.integer64 color_vctr
vec_cast.integer64.color_vctr <- function(x, to, ...)  coerce_color_vctr(x, to)



#' @export
vec_cast.color_vctr.character <- function(x, to, ...)  coerce_to_color_vctr(x, to)
#' @export
#' @method vec_cast.character color_vctr
vec_cast.character.color_vctr <- function(x, to, ...) coerce_color_vctr(x, to)

#' @export
vec_cast.color_vctr.factor <- function(x, to, ...)  coerce_to_color_vctr(x, to)
#' @export
#' @method vec_cast.factor color_vctr
vec_cast.factor.color_vctr <- function(x, to, ...) coerce_color_vctr(x, to)

#' @export
vec_cast.color_vctr.logical <- function(x, to, ...)  coerce_to_color_vctr(x, to)
#' @export
#' @method vec_cast.logical color_vctr
vec_cast.logical.color_vctr <- function(x, to, ...) coerce_color_vctr(x, to)

#' @export
vec_cast.color_vctr.list <- function(x, to, ...) list(x)
#' @export
#' @method vec_cast.list color_vctr
vec_cast.list.color_vctr <- function(x, to, ...) list(x)

#' @export
vec_cast.color_vctr.Date <- function(x, to, ...)  coerce_to_color_vctr(x, to)
#' @export
#' @method vec_cast.Date color_vctr
vec_cast.Date.color_vctr <- function(x, to, ...)  coerce_color_vctr(x, to)

#' @export
vec_cast.color_vctr.difftime <- function(x, to, ...)  coerce_to_color_vctr(x, to)
#' @export
#' @method vec_cast.difftime color_vctr
vec_cast.difftime.color_vctr <- function(x, to, ...)  coerce_color_vctr(x, to)

#' @export
vec_cast.color_vctr.POSIXct<- function(x, to, ...)  coerce_to_color_vctr(x, to)
#' @export
#' @method vec_cast.POSIXct color_vctr
vec_cast.POSIXct.color_vctr <- function(x, to, ...)  coerce_color_vctr(x, to)

#' @export
vec_cast.color_vctr.POSIXlt<- function(x, to, ...)  coerce_to_color_vctr(x, to)
#' @export
#' @method vec_cast.POSIXlt color_vctr
vec_cast.POSIXlt.color_vctr <- function(x, to, ...)  coerce_color_vctr(x, to)

#' @export
vec_cast.color_vctr.complex <- function(x, to, ...)  coerce_to_color_vctr(x, to)
#' @export
#' @method vec_cast.complex color_vctr
vec_cast.complex.color_vctr <- function(x, to, ...)  coerce_color_vctr(x, to)

#' @export
vec_cast.color_vctr.AsIs <- function(x, to, ...)  coerce_to_color_vctr(x, to)

#' @export
vec_cast.color_vctr.raw <- function(x, to, ...)  coerce_to_color_vctr(x, to)
#' @export
#' @method vec_cast.raw color_vctr
vec_cast.raw.color_vctr <- function(x, to, ...)  coerce_color_vctr(x, to)

#' @export
vec_cast.color_vctr.ordered <- function(x, to, ...)  coerce_to_color_vctr(x, to)
#' @export
#' @method vec_cast.raw color_vctr
vec_cast.ordered.color_vctr <- function(x, to, ...)  coerce_color_vctr(x, to)

coerce_color_vctr <- function(x, to){
  text_color <- field(x, ".text_color")
  background <- field(x, ".background")
  style <- field(x, ".style")
  x <- field(x, "vctr")

  x <- vec_c(x, get_ptype2(x, to))
  color_vctr(x, text_color = text_color, background = background, style = style)
}

coerce_to_color_vctr <- function(x, to){
  x <- vec_cast(x, get_ptype2(x, to))
  color_vctr(x)
}
