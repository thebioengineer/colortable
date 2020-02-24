#' @title Valid Coloring/Styling Options for {{Colortable}}
#'
#' @param method The output method type. One of "latex","html", or "console"
#'
#' @return vector of valid settings
#'
#' @name Valid
#' @aliases valid_text_color valid_background valid_style
#' @usage
#' valid_text_color
#' valid_background
#' valid_style
#'
NULL

#' Return vector of valid text color options
#' @rdname Valid
#' @export
valid_text_color <- function(method = c("latex","html","console")){
  method <- match.arg(method)
  styles <- eval(parse(text = paste0("names(",method,"_style_codes)")))

  pattern <- "^text[.]"
  gsub(pattern,"",grep(pattern,styles,value = TRUE))
}

#' Return vector of valid background color options
#' @rdname Valid
#' @export
valid_background <- function(method = c("latex","html","console")){
  method <- match.arg(method)
  styles <- eval(parse(text = paste0("names(",method,"_style_codes)")))

  pattern <- "^bg[.]"
  gsub(pattern,"",grep(pattern,styles,value = TRUE))
}

#' Return vector of valid text styling options
#' @rdname Valid
#' @export
valid_style <- function(method = c("latex","html","console")){
  method <- match.arg(method)
  styles <- eval(parse(text = paste0("names(",method,"_style_codes)")))

  pattern <- "^style[.]"
  gsub(pattern,"",grep(pattern,styles,value = TRUE))
}
