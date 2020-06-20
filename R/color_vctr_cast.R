
#' @export
vec_cast.color_vctr.color_vctr <- function(x, to, ...) {
  x
}

#' @export
vec_cast.color_vctr.double <- function(x, to, ...) color_vctr(x)
#' @export
#' @method vec_cast.double color_vctr
vec_cast.double.color_vctr <- function(x, to, ...) x

#' @export
vec_cast.color_vctr.integer <- function(x, to, ...) color_vctr(x)
#' @export
#' @method vec_cast.integer color_vctr
vec_cast.integer.color_vctr <- function(x, to, ...) x

#' @export
vec_cast.color_vctr.integer64 <- function(x, to, ...) color_vctr(x)
#' @export
#' @method vec_cast.integer64 color_vctr
vec_cast.integer64.color_vctr <- function(x, to, ...) x

#' @export
vec_cast.color_vctr.character <- function(x, to, ...) color_vctr(x)
#' @export
#' @method vec_cast.character color_vctr
vec_cast.character.color_vctr <- function(x, to, ...) x

#' @export
vec_cast.color_vctr.logical <- function(x, to, ...) color_vctr(x)
#' @export
#' @method vec_cast.logical color_vctr
vec_cast.logical.color_vctr <- function(x, to, ...) x


#' @export
#' @method vec_cast.data.frame color_vctr
#' @importFrom tibble as_tibble
vec_cast.data.frame.color_vctr <- function(x, to, ...) as_tibble(x)


#' @export
vec_cast.color_vctr.list <- function(x, to, ...) flatten_to_color_vctr(x)
#' @export
#' @method vec_cast.list color_vctr
vec_cast.list.color_vctr <- function(x, to, ...) lapply(seq_along(x),function(idx,x){x[idx]},x)

#' @export
vec_cast.color_vctr.Date <- function(x, to, ...) color_vctr(x)
#' @export
#' @method vec_cast.Date color_vctr
vec_cast.Date.color_vctr <- function(x, to, ...) x

#' @export
vec_cast.color_vctr.difftime <- function(x, to, ...) color_vctr(x)
#' @export
#' @method vec_cast.difftime color_vctr
vec_cast.difftime.color_vctr <- function(x, to, ...) x

#' @export
vec_cast.color_vctr.POSIXct<- function(x, to, ...) color_vctr(x)
#' @export
#' @method vec_cast.POSIXct color_vctr
vec_cast.POSIXct.color_vctr <- function(x, to, ...) x

#' @export
vec_cast.color_vctr.POSIXlt<- function(x, to, ...) color_vctr(x)
#' @export
#' @method vec_cast.POSIXlt color_vctr
vec_cast.POSIXlt.color_vctr <- function(x, to, ...) x

#' @export
vec_cast.color_vctr.complex <- function(x, to, ...) color_vctr(x)
#' @export
#' @method vec_cast.complex color_vctr
vec_cast.complex.color_vctr <- function(x, to, ...) x



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
