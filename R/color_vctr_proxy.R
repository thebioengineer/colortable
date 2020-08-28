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
