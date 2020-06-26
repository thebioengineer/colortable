
#' @export
vec_ptype2.color_vctr.color_vctr <- function(x, y, ...){
  data_type <- get_ptype2(x, y, ...)
  new_color_vctr(data_type)
}

#' @export
#' @method vec_ptype2.double color_vctr
vec_ptype2.double.color_vctr <- vec_ptype2.color_vctr.color_vctr
#' @export
vec_ptype2.color_vctr.double <- vec_ptype2.color_vctr.color_vctr

#' @export
#' @method vec_ptype2.integer color_vctr
vec_ptype2.integer.color_vctr    <- vec_ptype2.color_vctr.color_vctr
#' @export
vec_ptype2.color_vctr.integer    <- vec_ptype2.color_vctr.color_vctr

#' @export
#' @method vec_ptype2.integer64 color_vctr
vec_ptype2.integer64.color_vctr    <- vec_ptype2.color_vctr.color_vctr
#' @export
vec_ptype2.color_vctr.integer64    <- vec_ptype2.color_vctr.color_vctr

#' @export
#' @method vec_ptype2.integer64 color_vctr
vec_ptype2.complex.color_vctr    <- vec_ptype2.color_vctr.color_vctr
#' @export
vec_ptype2.color_vctr.complex    <- vec_ptype2.color_vctr.color_vctr

#' @export
#' @method vec_ptype2.character color_vctr
vec_ptype2.character.color_vctr <- vec_ptype2.color_vctr.color_vctr
#' @export
vec_ptype2.color_vctr.character <- vec_ptype2.color_vctr.color_vctr

#' @export
#' @method vec_ptype2.factor color_vctr
vec_ptype2.factor.color_vctr <- vec_ptype2.color_vctr.color_vctr
#' @export
vec_ptype2.color_vctr.factor <- vec_ptype2.color_vctr.color_vctr

#' @export
#' @method vec_ptype2.logical color_vctr
vec_ptype2.logical.color_vctr <- vec_ptype2.color_vctr.color_vctr
#' @export
vec_ptype2.color_vctr.logical <- vec_ptype2.color_vctr.color_vctr

#' @export
#' @method vec_ptype2.list color_vctr
vec_ptype2.list.color_vctr <- function(x, y, ...) list()
#' @export
vec_ptype2.color_vctr.list <- function(x, y, ...) list()

#' @export
#' @method vec_ptype2.Date color_vctr
vec_ptype2.Date.color_vctr <- vec_ptype2.color_vctr.color_vctr
#' @export
vec_ptype2.color_vctr.Date <- vec_ptype2.color_vctr.color_vctr

#' @export
vec_ptype2.color_vctr.difftime <- vec_ptype2.color_vctr.color_vctr
#' @export
#' @method vec_ptype2.difftime color_vctr
vec_ptype2.difftime.color_vctr <- vec_ptype2.color_vctr.color_vctr

#' @export
vec_ptype2.color_vctr.POSIXct<-vec_ptype2.color_vctr.color_vctr
#' @export
#' @method vec_ptype2.POSIXct color_vctr
vec_ptype2.POSIXct.color_vctr <- vec_ptype2.color_vctr.color_vctr

#' @export
vec_ptype2.color_vctr.POSIXlt<- vec_ptype2.color_vctr.color_vctr
#' @export
#' @method vec_ptype2.POSIXlt color_vctr
vec_ptype2.POSIXlt.color_vctr <- vec_ptype2.color_vctr.color_vctr

#' @export
vec_ptype2.color_vctr.complex <- vec_ptype2.color_vctr.color_vctr
#' @export
#' @method vec_ptype2.complex color_vctr
vec_ptype2.complex.color_vctr <- vec_ptype2.color_vctr.color_vctr

#' @export
vec_ptype2.color_vctr.AsIs <- vec_ptype2.color_vctr.color_vctr
#' @export
#' @method vec_ptype2.AsIs color_vctr
vec_ptype2.AsIs.color_vctr <- vec_ptype2.color_vctr.color_vctr

#' @export
vec_ptype2.color_vctr.raw <- vec_ptype2.color_vctr.color_vctr
#' @export
#' @method vec_ptype2.raw color_vctr
vec_ptype2.raw.color_vctr <- vec_ptype2.color_vctr.color_vctr

#' @export
vec_ptype2.color_vctr.ordered <- vec_ptype2.color_vctr.color_vctr
#' @export
#' @method vec_ptype2.raw color_vctr
vec_ptype2.ordered.color_vctr <- vec_ptype2.color_vctr.color_vctr

get_ptype2 <- function(x, y, ...){
  if(is_color_vctr(x)){
    x <- field(x, "vctr")
  }
  if(is_color_vctr(y)){
    y <- field(y, "vctr")
  }
  vec_ptype2(x, y, ...)
}
