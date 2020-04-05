
#' @title Determine Printing for colortable
#' @description determines if table is being printed interactively, or in an rmd
#' @return character vector
#' @export
print_method <- function() {

  is_notebook <- isTRUE(getOption("rstudio.notebook.executing"))

  if (interactive() & !is_notebook) {
    "console"
  } else if (is_notebook) {
    "html"
  } else{
    knitr::opts_knit$get('rmarkdown.pandoc.to')
  }
}


#' @title Access Attributes consistent with vctrs
#' @description provide a utility to access contents of colorvctrs consistent with vctrs
#' @return specified attribute
#' @param x color_vctr object
#' @param i attribute to access
#' @param idx subset of attributes to return. if missing, assumes all
field <- function(x, i){
  if (missing(i)) {
    stop("Prove an attribute name or 'vctr'")
  }

  if (i == "vctr") {
    x_attr <- attributes(x)
    x_attr[c(".text_color",".background",".style")] <- NULL
    x_attr["class"] <- setdiff(x_attr[["class"]],"color_vctr")
    attributes(x) <- x_attr
    x
  } else{
    attr(x, i, exact = TRUE)
  }
}
