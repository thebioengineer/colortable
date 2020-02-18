
#' @title Determine Printing for colortable
#' @description determines if table is being printed interactively, or in an rmd
#' @return character vector
#' @export
print_method <- function(){
  if(interactive()){
    "console"
  }else{
    knitr::opts_knit$get('rmarkdown.pandoc.to')
  }
}

