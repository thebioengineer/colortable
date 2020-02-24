#' @title Valid Coloring/Styling Options for {{Colortable}}
#' @return vector of valid settings
#' @param method The output method type. One of "latex","html", or "console"
#' @name Valid_Styles
#' @aliases valid_text_color valid_background valid_style
#' @usage
#' valid_text_color
#' valid_background
#' valid_style
NULL

#' @export
valid_text_color <- function(method = c("latex","html","console")){
  method <- match.arg(method)
  styles <- eval(parse(text = paste0("names(",method,"_style_codes)")))

  pattern <- "^text[.]"
  gsub(pattern,"",grep(pattern,styles,value = TRUE))
}

#' @export
valid_background <- function(method = c("latex","html","console")){
  method <- match.arg(method)
  styles <- eval(parse(text = paste0("names(",method,"_style_codes)")))

  pattern <- "^bg[.]"
  gsub(pattern,"",grep(pattern,styles,value = TRUE))
}

#' @export
valid_style <- function(method = c("latex","html","console")){
  method <- match.arg(method)
  styles <- eval(parse(text = paste0("names(",method,"_style_codes)")))

  pattern <- "^style[.]"
  gsub(pattern,"",grep(pattern,styles,value = TRUE))
}
