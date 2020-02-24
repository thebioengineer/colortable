#' Is the object a color_vctr?
#' Detect if object is a colortable vector
#'
#' @param x object to be checked if is a color_vctr
#'
#' @export
is_color_vctr <- function(x){
  inherits(x,"color_vctr")
}

#' @export
`[.color_vctr`<-function(x,i){

  value <- unclass(x)[i]
  style <- attr(x,".style")[i]
  text_color <- attr(x,".text_color")[i]
  background <- attr(x,".background")[i]

  as_color_vctr(
    value,
    style = style,
    text_color = text_color,
    background = background)
}


#' @export
`[<-.color_vctr` <- function(x, i, value){

  if (is_color_vctr(value)) {

    if (!(length(i) == length(value) | length(value) == 1)){
      warning("number of items to replace is not a multiple of replacement length")
      i <- i[seq(length(value))]
    }

    for (idx in seq_along(i)) {
      idx_i <- i[idx]
      idx_val <- idx
      if (length(value) == 1)
        idx_val <-  1
      x <- append_colortable_vect(x,idx_i,value[idx_val])
    }

  } else {
    value <- as_color_vctr(value)
    x[i] <- value
  }
  x
}


append_colortable_vect <- function(x,i,value){

  vect <- .subset(x,seq_along(x))
  text_color <- attr(x,".text_color")
  background <- attr(x,".background")
  style <- attr(x,".style")

  vect[i] <- .subset(value,1)
  text_color[i] <- attr(value,".text_color")
  background[i] <- attr(value,".background")
  style[i] <- attr(value,".style")

  return(
    new_color_vctr(
      vect,
      text_color = text_color,
      background = background,
      style = style)
  )
}
