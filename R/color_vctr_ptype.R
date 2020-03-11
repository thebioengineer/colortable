
#' @export vec_ptype2.color_vctr
#' @method vec_ptype2 color_vctr
#' @importFrom vctrs vec_ptype2
#' @export
vec_ptype2.color_vctr <- function(x, y, ...) UseMethod("vec_ptype2.color_vctr", y)

#' @method vec_ptype2.color_vctr default
#' @importFrom vctrs vec_default_ptype2
#' @export
vec_ptype2.color_vctr.default <- function(x, y, ..., x_arg = "x", y_arg = "y"){
  vec_default_ptype2(x, y, x_arg = x_arg, y_arg = y_arg)
}

#' @export
#' @method vec_ptype2.color_vctr color_vctr
vec_ptype2.color_vctr.color_vctr <- function(x, y, ...) new_color_vctr()


#' @method vec_ptype2.character color_vctr
#' @importFrom vctrs vec_ptype2.character
#' @export
vec_ptype2.character.color_vctr <- function(x, y, ...) color_vctr()
#' @method vec_ptype2.color_vctr character
#' @export
vec_ptype2.color_vctr.character <- function(x, y, ...) color_vctr()




#' @importFrom vctrs vec_ptype2.double
#' @method vec_ptype2.double color_vctr
#' @export
vec_ptype2.double.color_vctr <- function(x, y, ...) color_vctr()
#' @method vec_ptype2.color_vctr double
#' @export
vec_ptype2.color_vctr.double <- function(x, y, ...) color_vctr()

#' @export
#' @method vec_ptype2.color_vctr integer
vec_ptype2.color_vctr.integer    <- function(x, y, ...) color_vctr()
#' @export
#' @method vec_ptype2.integer color_vctr
#' @importFrom vctrs vec_ptype2.integer
vec_ptype2.integer.color_vctr    <- function(x, y, ...) color_vctr()


#' @export
#' @method vec_ptype2.color_vctr logical
vec_ptype2.color_vctr.logical    <- function(x, y, ...) color_vctr()
#' @export
#' @method vec_ptype2.logical color_vctr
#' @importFrom vctrs vec_ptype2.logical
vec_ptype2.logical.color_vctr    <- function(x, y, ...) color_vctr()

