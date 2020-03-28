
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


