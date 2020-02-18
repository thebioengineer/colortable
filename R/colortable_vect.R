
#' @exportClass colortable_vect
new_colortable_vect <- function(vect, text_color = NA, background = NA, style = NA ){

  stopifnot(length(text_color) == 1 | length(text_color) == length(vect))
  stopifnot(length(background) == 1 | length(background) == length(vect))
  stopifnot(length(style) == 1 | length(style) == length(vect))

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
      class = "colortable_vect"
    )
  )
}


#' @title vector of colortable_cells
#' @description `c` destroys the attributes of the contents, colortable_vect preserves them
#' @param ... colortable_cells
#' @return a vector of colortable_cells
#' @export

colortable_vect <- function(x,...,text_color = NA, background = NA, style = NA){
  UseMethod("colortable_vect",x)
}

#' @export
colortable_vect.default <- function(x,...,text_color = NA, background = NA, style = NA) {
  as_colortable_vect(list(x,...),text_color = NA, background = NA, style = NA)
}

#' @export
colortable_vect.list <- function(x,...){

  vect <- sapply(x,`[`,1)
  text_color <- sapply(x,attr,".text_color")
  background <- sapply(x,attr,".background")
  style <- sapply(x,attr,".style")

  return(
    new_colortable_vect(
    vect,
    text_color = text_color,
    background = background,
    style = style))
}

#' @export
colortable_vect.colortable_cell <- function(x,...){

  vect <- sapply(list(x,...),'c')
  text_color <- sapply(list(x,...),".text_color")
  background <- sapply(list(x,...),attr,".background")
  style <- sapply(list(x,...),attr,".style")

  return(new_colortable_vect(
    vect,
    text_color = text_color,
    background = background,
    style = style))
}

#' @export
colortable_vect.colortable_vect <- function(x,...){

  coltable_nect_list <- list(x,...)

  vect <- do.call('c',lapply(coltable_nect_list, function(z){.subset(z,seq_along(z))}))
  text_color <- do.call('c',lapply(coltable_nect_list,attr,".text_color"))
  background <- do.call('c',lapply(coltable_nect_list,attr,".background"))
  style <- do.call('c',lapply(coltable_nect_list,attr,".style"))

  return(new_colortable_vect(
    vect,
    text_color = text_color,
    background = background,
    style = style))
}

#' @export
#'
as_colortable_vect <- function(...,text_color = NA, background = NA, style = NA){

  new_cells <- as.list(...)

  stopifnot(length(text_color) == 1 | length(text_color) == length(new_cells))
  stopifnot(length(background) == 1 | length(background) == length(new_cells))
  stopifnot(length(style) == 1 | length(style) == length(new_cells))


  cell_list <- lapply(seq_along(new_cells),function(idx, new_cells, text_color, background, style){

    text_color = ifelse(length(text_color)==1,text_color, style[idx])
    background = ifelse(length(background)==1,background, background[idx])
    style      = ifelse(length(style)==1,style, style[idx])

    new_cell(new_cells[[idx]],
             text_color = text_color,
             background = background,
             style = style)
  },new_cells, text_color, background, style)

  colortable_vect(cell_list)
}



#' @title Is a colortable_vect
#' @description detect if object is a colortable vector
#' @export
is_colortable_vect <- function(x){
  inherits(x,"colortable_vect")
}

#' @export
`[.colortable_vect`<-function(x,i){

  vect <- lapply(i,function(idx,vec){

    if (idx <= length(vec)) {
      value <- .subset(vec,idx)
      style <- attr(vec,".style")[idx]
      text_color <- attr(vec,".text_color")[idx]
      background <- attr(vec,".background")[idx]

    }else{
      value <- style <- text_color <- background <- NA
    }

    as_colortable_cell(
      value,
      style = style,
      text_color = text_color,
      background = background)

  },x)

  do.call('colortable_vect',vect)
}


#' @export
`[<-.colortable_vect` <- function(x, i, value){

  if (is_colortable_vect(value)) {

    if (!(length(i) == length(value) | length(value) == 1)){
      warning("number of items to replace is not a multiple of replacement length")
      i <- i[seq(length(value))]
    }

    for (idx in seq_along(i)) {
      idx_i <- i[idx]
      idx_val <- idx
      if (length(value) == 1)
        idx_val <-  1
      x <- append_colortable_cell(x,idx_i,value[idx_val])
    }

  } else {
    value <- as_colortable_vect(value)
    x[i] <- value
  }
  x
}

append_colortable_cell <- function(x,i,value){

  vect <- .subset(x,seq_along(x))
  text_color <- attr(x,".text_color")
  background <- attr(x,".background")
  style <- attr(x,".style")

  vect[i] <- .subset(value,1)
  text_color[i] <- attr(value,".text_color")
  background[i] <- attr(value,".background")
  style[i] <- attr(value,".style")

  return(
    new_colortable_vect(
      vect,
      text_color = text_color,
      background = background,
      style = style)
    )
}

#' @export
as.list.colortable_vect <- function(x,...){
  lapply(seq_along(x),function(idx,vect){vect[idx]},x)
}

#' @export
as.data.frame.colortable_vect <- function (
  x, row.names = NULL, optional = FALSE, ..., nm = paste(deparse(substitute(x), width.cutoff = 500L), collapse = " "))
{

  text_color <- attr(x,".text_color")
  background <- attr(x,".background")
  style <- attr(x,".style")
  x <- .subset(x,seq_along(x))


  df <- as.data.frame(list(x = x))
  attr(df, ".text_color") <- text_color
  attr(df, ".background") <- background
  attr(df, ".style") <- style
  class(df) <- c("colortable","data.frame")
  return(df)
}
