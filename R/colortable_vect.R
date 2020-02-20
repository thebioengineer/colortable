
#' @exportClass color_vctr
new_color_vctr <- function(vect, text_color = NA, background = NA, style = NA ){

  stopifnot(length(text_color) == 1 | length(text_color) == length(vect))
  stopifnot(length(background) == 1 | length(background) == length(vect))
  stopifnot(length(style) == 1 | length(style) == length(vect))

  stopifnot(is.vector(vect, mode = "any"))

  if(length(text_color) == 1)
    text_color <- rep(text_color,length(vect))
  if(length(background) == 1)
    background <- rep(background,length(vect))
  if(length(style) == 1)
    style <- rep(style,length(vect))

  return(
    structure(
      vect,
      ".text_color" = text_color,
      ".background" = background,
      ".style" = style,
      class = "color_vctr"
    )
  )
}


#' @title vector of colortable_cells
#' @description `c` destroys the attributes of the contents, color_vctr preserves them
#' @param ... colortable_cells
#' @return a vector of colortable_cells
#' @export

color_vctr <- function(x,...,text_color = NA, background = NA, style = NA){
  UseMethod("color_vctr",x)
}

#' @export
color_vctr.default <- function(x,...,text_color = NA, background = NA, style = NA) {
  as_color_vctr(c(x,list(...)),text_color =text_color, background = background, style = style)
}

#' @export
color_vctr.list <- function(x,...){

  vect <- sapply(x,`[`,1)
  text_color <- sapply(x,attr,".text_color")
  background <- sapply(x,attr,".background")
  style <- sapply(x,attr,".style")

  return(
    new_color_vctr(
    vect,
    text_color = text_color,
    background = background,
    style = style))
}

#' @export
color_vctr.color_vctr <- function(x,...){

  coltable_nect_list <- list(x,...)

  vect <- do.call('c',lapply(coltable_nect_list, function(z){.subset(z,seq_along(z))}))
  text_color <- do.call('c',lapply(coltable_nect_list,attr,".text_color"))
  background <- do.call('c',lapply(coltable_nect_list,attr,".background"))
  style <- do.call('c',lapply(coltable_nect_list,attr,".style"))

  return(new_color_vctr(
    vect,
    text_color = text_color,
    background = background,
    style = style))
}

#' @export
#'
as_color_vctr <- function(...,text_color = NA, background = NA, style = NA){

  new_vect <- unlist(c(...))

  stopifnot(length(text_color) == 1 | length(text_color) == length(new_vect))
  stopifnot(length(background) == 1 | length(background) == length(new_vect))
  stopifnot(length(style) == 1 | length(style) == length(new_vect))

  new_color_vctr(new_vect,
             text_color = text_color,
             background = background,
             style = style)
}



#' @title Is a color_vctr
#' @description detect if object is a colortable vector
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

#' @export
as.list.color_vctr <- function(x,...){
  lapply(seq_along(x),function(idx,vect){vect[idx]},x)
}

#' @export
as.data.frame.color_vctr <- function (
  x, row.names = NULL, optional = FALSE, ..., nm = paste(deparse(substitute(x), width.cutoff = 500L), collapse = " ")){

  text_color <- attr(x,".text_color")
  background <- attr(x,".background")
  style <- attr(x,".style")
  x <- .subset(x,seq_along(x))


  df <- as.data.frame(list(x = x))
  attr(df, ".text_color") <- text_color
  attr(df, ".background") <- background
  attr(df, ".style") <- style
  class(df) <- c("color_table","data.frame")
  return(df)
}
