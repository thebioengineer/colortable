
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
  set_styling(as_colortable_vect(x), idx, text_color, background, style)
}

#' @export
set_styling.colortable_vect <- function(x, idx = rep(TRUE, length(x)), text_color = NA, background = NA, style = NA){

  #if is logical, it must be the sample length as x
  if (is.logical(idx)) {
     if (length(idx) != length(x)) {
        stop("Length of index must be same as input vector.")
     }
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
  old_text_color <- attr(x,".text_color")
  old_background <- attr(x,".background")
  old_style <- attr(x,".style")

  old_text_color[idx] <- text_color
  old_background[idx] <- background
  old_style[idx] <- style

  attr(x,".text_color") <- old_text_color
  attr(x,".background") <- old_background
  attr(x,".style") <- old_style

  return(x)
}

