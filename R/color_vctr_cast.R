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
vec_cast.color_vctr.color_vctr <- function(x, to, ...) {
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
#' @method vec_cast.color_vctr list
vec_cast.color_vctr.list <- function(x, to, ...) flatten_to_color_vctr(x)
#' @export
#' @method vec_cast.list color_vctr
#' @importFrom vctrs vec_cast.list
vec_cast.list.color_vctr <- function(x, to, ...) lapply(seq_along(x),function(idx,x){x[idx]},x)


flatten_to_color_vctr <- function(x){
  envir <- new.env()
  envir$flattened <- color_vctr()
  flatten_to_color_vctr_exec(x,envir)
  envir$flattened
}

flatten_to_color_vctr_exec <- function(x, envir = new.env()) {
  if (!exists("flattend", envir = envir)) {
    envir$flattened <- color_vctr()
  }
  lapply(x, function(z) {
    check_atomic <- try(vec_assert(z, atomic(z)), silent = TRUE)
    if (inherits(check_atomic, "try-error")) {
      if (any(c("data.frame", "list") %in% class(z))) {
        flatten_to_color_vctr_exec(z, envir = envir)
      } else{
        stop("Error converting object of type `",
             class(z),
             "` to a color_vctr.")
      }
    } else{
      envir$flattened <- c(envir$flattened, vec_cast(z, color_vctr()))
    }
  })
}
