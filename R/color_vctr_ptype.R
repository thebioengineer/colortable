
#' @export
vec_ptype2.color_vctr.color_vctr <- function(x, y, ...) new_color_vctr()


#' @export
#' @method vec_ptype2.character color_vctr
vec_ptype2.character.color_vctr <- function(x, y, ...) new_color_vctr()
#' @export
vec_ptype2.color_vctr.character <- function(x, y, ...) new_color_vctr()

#' @export
#' @method vec_ptype2.double color_vctr
vec_ptype2.double.color_vctr <- function(x, y, ...) new_color_vctr()
#' @export
vec_ptype2.color_vctr.double <- function(x, y, ...) new_color_vctr()

#' @export
#' @method vec_ptype2.integer color_vctr
vec_ptype2.integer.color_vctr    <- function(x, y, ...) new_color_vctr()
#' @export
vec_ptype2.color_vctr.integer    <- function(x, y, ...) new_color_vctr()

#' @export
#' @method vec_ptype2.logical color_vctr
vec_ptype2.logical.color_vctr    <- function(x, y, ...) new_color_vctr()
#' @export
vec_ptype2.color_vctr.logical    <- function(x, y, ...) new_color_vctr()

#' @export
#' @method vec_ptype2.list color_vctr
vec_ptype2.list.color_vctr    <- function(x, y, ...) list()#' @export
#' @export
vec_ptype2.color_vctr.list    <- function(x, y, ...) list()

