

#' @export vec_math.color_vctr
#' @export
#' @method vec_math color_vctr
#' @importFrom vctrs field vec_math
vec_math.color_vctr <- function(.fn, .x, ...) {
  vec_math_base(.fn, field(.x, "vctr"), ...)
}


#' Arithmatic
#' @export vec_arith.color_vctr
#' @export
#' @method vec_arith color_vctr
#' @importFrom vctrs field vec_arith
vec_arith.color_vctr <- function(op, x, y, ...) {
  UseMethod("vec_arith.color_vctr", y)
}
#' @importFrom vctrs stop_incompatible_op
vec_arith.color_vctr.default <- function(op, x, y, ...) {
  stop_incompatible_op(op, x, y)
}

#' @export vec_arith.color_vctr.color_vctr
#' @export
#' @method vec_arith.color_vctr MISSING
#' @importFrom vctrs field
#'
#'
#'

vec_arith.color_vctr.color_vctr <- function(op, x, y, ...) {
  if (as.character(op) %in% c("+", "-", "/", "*", "^", "%%", "%/%", "!", "&", "|")) {
    return(vec_arith.color_vctr.color_vctr.op(op, x, y))
  } else{
    stop_incompatible_op(op, x, y)
  }
}

vec_arith.color_vctr.color_vctr.op <-
  function(op, x, y, type = c("left", "right")) {
    type <- match.arg(type)

    op <- getFunction(as.character(op))

    res_value <- op(field(x, "vctr"), field(y, "vctr"))

    res_text_color <-
      merge_styling(field(x, ".text_color"), field(y, ".text_color"), type)
    res_background <-
      merge_styling(field(x, ".background"), field(y, ".background"), type)
    res_style <-
      merge_styling(field(x, ".style"), field(y, ".style"), type)

    new_color_vctr(
      res_value,
      text_color = res_text_color,
      background = res_background,
      style = res_style
    )

  }


merge_styling <- function(x, y, type = c("left", "right")) {
  type <- match.arg(type)
  x <- vec_recycle(x, max(c(length(x), length(y))))
  y <- vec_recycle(y, max(c(length(x), length(y))))
  idx <- ifelse(type == "left", is.na(x),!is.na(y))
  x[idx] <- y[idx]
  x
}


#' @export vec_arith.color_vctr.MISSING
#' @export
#' @method vec_arith.color_vctr MISSING
#' @importFrom vctrs field
vec_arith.color_vctr.MISSING <- function(op, x, y, ...) {
  switch(
    op,
    `-` = new_color_vctr(
      field(x, "vctr") * -1,
      field(x, ".text_color"),
      field(x, ".background"),
      field(x, ".style")
    ),
    `+` = x,
    stop_incompatible_op(op, x, y)
  )
}
