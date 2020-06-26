#' @export
#' @method vec_math color_vctr
#' @importFrom vctrs field vec_math
vec_math.color_vctr <- function(.fn, .x, ...) {
  vec_math_base(.fn, field(.x, "vctr"), ...)
}


#' Arithmatic
#' @export
#' @inheritParams vctrs::vec_arith
#' @method vec_arith color_vctr
#' @importFrom vctrs field vec_arith
vec_arith.color_vctr <- function(op, x, y, ...) {
  UseMethod("vec_arith.color_vctr", y)
}
#' @importFrom vctrs stop_incompatible_op
vec_arith.color_vctr.default <- function(op, x, y, ...) {
  stop_incompatible_op(op, x, y)
}

#' @export
#' @method vec_arith.color_vctr color_vctr
#' @importFrom vctrs field
vec_arith.color_vctr.color_vctr <- function(op, x, y, ...) {
  if (as.character(op) %in% c("+", "-", "/", "*", "^", "%%", "%/%", "!", "&", "|")) {
    return(vec_arith.color_vctr.color_vctr.op(op, x, y))
  } else{
    stop_incompatible_op(op, x, y)
  }
}

#' @export
#' @method vec_arith.color_vctr numeric
#' @importFrom vctrs field
vec_arith.color_vctr.numeric <- function(op, x, y, ...) {
  if (as.character(op) %in% c("+", "-", "/", "*", "^", "%%", "%/%", "!", "&", "|")) {
    return(vec_arith.color_vctr.color_vctr.op(op, x, color_vctr(y)))
  } else{
    stop_incompatible_op(op, x, y)
  }
}

#' @export
#' @method vec_arith.numeric color_vctr
#' @importFrom vctrs field vec_arith.numeric
vec_arith.numeric.color_vctr <- function(op, x, y, ...) {
  if (as.character(op) %in% c("+", "-", "/", "*", "^", "%%", "%/%", "&", "|")) {
    return(vec_arith.color_vctr.color_vctr.op(op, color_vctr(x), y))
  } else{
    stop_incompatible_op(op, x, y)
  }
}


#' @export
#' @method vec_arith.color_vctr logical
#' @importFrom vctrs field
vec_arith.color_vctr.logical <- function(op, x, y, ...) {
  if (as.character(op) %in% c("+", "-", "/", "*", "^", "%%", "%/%", "!", "&", "|")) {
    return(vec_arith.color_vctr.color_vctr.op(op, x, color_vctr(y)))
  } else{
    stop_incompatible_op(op, x, y)
  }
}

#' @export
#' @method vec_arith.logical color_vctr
#' @importFrom vctrs field vec_arith.logical
vec_arith.logical.color_vctr <- function(op, x, y, ...) {
  if (as.character(op) %in% c("+", "-", "/", "*", "^", "%%", "%/%", "&", "|")) {
    return(vec_arith.color_vctr.color_vctr.op(op, color_vctr(x), y))
  } else{
    stop_incompatible_op(op, x, y)
  }
}

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
    `!` = new_color_vctr(
      !field(x, "vctr"),
      field(x, ".text_color"),
      field(x, ".background"),
      field(x, ".style")
    ),
    stop_incompatible_op(op, x, y)
  )
}


#' @importFrom methods getFunction
vec_arith.color_vctr.color_vctr.op <- function(op, x, y ) {

  op <- getFunction(as.character(op))

  res_value <- op(vec_cast(field(x, "vctr"), field(x, "vctr")),
                  vec_cast(field(y, "vctr"), field(y, "vctr")))

  res_styling <- merge_styling( x, y)

  color_vctr(
    res_value,
    text_color = res_styling[[".text_color"]],
    background = res_styling[[".background"]],
    style = res_styling[[".style"]]
  )

}


#' Utilities to merge styling of two vectors
#'
#' @param x color_vctr
#' @param y color_vctr
#'
merge_styling <- function(x, y) {
  type <- getOption("colortable.precedence",default = "left")
  switch(type,
         left = merge_styling.dir(x,y),
         right = merge_styling.dir(y,x),
         mixed = merge_styling.mixed(x,y),
         blend = merge_styling.blend(x,y))
}

merge_styling.dir <- function(x,y){

  # identify where all the records are unstyled (NA)
  idx <- Reduce(`&`,lapply(setdiff(fields(x),"vctr"),function(fieldname){
    is.na(vec_recycle(field(x,fieldname), size = max(c(length(x), length(y)))))
  }))

  # replace cases of x where all fields are NA with y
  styling <- lapply(setdiff(fields(x),"vctr"),function(fieldname, idx){
    style <- vec_recycle(field(x,fieldname), size = max(c(length(x), length(y))))
    style[idx] <- vec_recycle(field(y,fieldname), size = max(c(length(x), length(y))))[idx]
    style
  },idx)

  names(styling) <- setdiff(fields(x),"vctr")

  styling
}

merge_styling.mixed <- function(x,y){

  # replace cases based on each field x where they are are NA with y
  styling <- lapply(setdiff(fields(x),"vctr"),function(fieldname){
    style <- vec_recycle(field(x,fieldname), size = max(c(length(x), length(y))))
    style[is.na(style)] <- vec_recycle(field(y,fieldname), size = max(c(length(x), length(y))))[is.na(style)]
    style
  })
  names(styling) <- setdiff(fields(x),"vctr")
  styling
}

merge_styling.blend <- function(x, y){
  stop("Method `merge_styling.blend` not implemented yes")
}
