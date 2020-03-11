#' @exports
#' @export vec_cast.color_vctr
#' @method vec_cast color_vctr
vec_cast.color_vctr <- function(x, to, ...) {
  UseMethod("vec_cast.color_vctr")
}

#' @method vec_cast.color_vctr default
#' @importFrom vctrs vec_default_cast
#' @export
vec_cast.color_vctr.default <- function(x, to, ...) vec_default_cast(x, to)

#' @export
#' @method vec_cast.color_vctr color_vctr
#' @importFrom vctrs vec_default_cast
vec_cast.color_vctr.color_vctr <- function(x, to, ..., x_arg = "x", to_arg = "to") {
  x
}

#' @export
#' @method vec_cast.color_vctr double
vec_cast.color_vctr.double <- function(x, to, ...) color_vctr(x)
#' @export
#' @method vec_cast.double color_vctr
#' @importFrom vctrs vec_cast.double
vec_cast.double.color_vctr <- function(x, to, ...) x



#' @export
#' @method vec_cast.color_vctr integer
vec_cast.color_vctr.integer <- function(x, to, ...) color_vctr(x)
#' @export
#' @method vec_cast.integer color_vctr
#' @importFrom vctrs vec_cast.integer
vec_cast.integer.color_vctr <- function(x, to, ...) x



#' @export
#' @method vec_cast.color_vctr character
vec_cast.color_vctr.character <- function(x, to, ...) color_vctr(x)
#' @export
#' @method vec_cast.character color_vctr
#' @importFrom vctrs vec_cast.character
vec_cast.character.color_vctr <- function(x, to, ...) x

#' @export
#' @method vec_cast.color_vctr logical
vec_cast.color_vctr.logical <- function(x, to, ...) color_vctr(x)

#' @export
#' @method vec_cast.logical color_vctr
#' @importFrom vctrs vec_cast.logical
vec_cast.logical.color_vctr <- function(x, to, ...) x

#' @export
#' @method vec_cast.data.frame color_vctr
#' @importFrom vctrs vec_cast.data.frame
vec_cast.data.frame.color_vctr <- function(x, to, ...) as_tibble(x)

#' @export
#' @method vec_cast.color_vctr logical
vec_cast.list.color_vctr <- function(x, to, ...) lapply(seq_along(x),function(idx,x){x[idx]},x)
