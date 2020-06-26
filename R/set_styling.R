
#' @title Update/Set styling of colortable elements
#' @param x input vector to be styled
#' @param idx the indexes to be updated or styled. Either numeric or logical
#' @param text_color a valid text color to set the elements
#' @param background a valid background color to set the elements
#' @param style a valid style to apply to the elements
#' @return colortable_vect with applied/updated styling
#' @export
#'
set_styling<- function(x, idx = rep(TRUE, length(x)), text_color = NA, background = NA, style = NA){
  UseMethod("set_styling")
}

#' @export
#'
set_styling.default <- function(x, idx = rep(TRUE, length(x)), text_color = NA, background = NA, style = NA){

  new_color <- rep(NA, length(x))
  new_background <- rep(NA, length(x))
  new_style <- rep(NA, length(x))

  new_color[idx] <- text_color
  new_background[idx] <- background
  new_style[idx] <- style

  color_vctr(x, text_color = new_color,background = new_background, style = new_style)
}

#' @export
set_styling.color_vctr <- function(x, idx = rep(TRUE, length(x)), text_color = NA, background = NA, style = NA){

  #if is logical, it must be the sample length as x
  if (is.logical(idx)) {
     if (length(idx) != length(x)) {
        stop("Length of index must be same as input vector.")
     }
    idx <- which(idx)
  } else if (is.numeric(idx)) {
    if (any(duplicated(idx))){
      warning("Duplicated indexes provided.")
      idx <- unique(idx)
    }
    if(any(idx > length(x)) | any(idx < 1)){
      stop("Indexes out of Range")
    }
  }else{
    stop("Invalid Index. Must be of type Numeric or Logical, not '",class(idx)[[1]],"'.")
  }

  # if is numeric, no duplicates
  x[idx] <- color_vctr(field(x,"vctr")[idx], text_color = text_color, background = background, style = style)

  return(x)
}

