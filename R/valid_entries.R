#' @title Valid Colors/Styling Options for {{Colortable}}
#'
#' @param method The output method type. One of "latex","html", or "console"
#'
#' @return vector of valid settings
#'
#' @name Valid
#' @aliases valid_colors  valid_style
#' @usage
#' valid_text_color
#' valid_style
#'
NULL

#' Return vector of valid text color options
#' @rdname Valid
#' @export
valid_colors <- function(method = c("latex","html","console")){
  method <- match.arg(method)
  styles <- eval(parse(text = paste0("color_key_",method)))[,c("Name","hex")]
  colnames(styles) <- c("Color Name","Hex Code")
  styles
}


#' Return vector of valid text styling options
#' @rdname Valid
#' @export
valid_style <- function(method = c("latex","html","console")){
  method <- match.arg(method)
  eval(parse(text = paste0("names(",method,"_style_codes)")))
}
