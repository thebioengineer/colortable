#' @export
as.list.color_vctr <- function(x,...){
  lapply(seq_along(x),function(idx,vect){vect[idx]},x)
}

#' @export
as.data.frame.color_vctr <- function (x, row.names = NULL, optional = FALSE, ...){

  nm = paste(deparse(substitute(x), width.cutoff = 500L), collapse = " ")
  force(nm)
  nrows <- length(x)
  text_color <- attr(x,".text_color")
  background <- attr(x,".background")
  style <- attr(x,".style")

  x <- list(x)


  if (is.null(row.names)) {
    if (nrows == 0L)
      row.names <- character()
    else if (length(row.names <- names(x)) != nrows || anyDuplicated(row.names))
      row.names <- .set_row_names(nrows)
  }

  if (!is.null(names(x)))
    names(x) <- NULL

  if (!optional)
    names(x) <- nm

  structure(x, row.names = row.names, class = "data.frame")
}
