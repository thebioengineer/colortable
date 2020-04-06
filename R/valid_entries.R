#' @title Valid Colors/Styling Options for {{Colortable}}
#'
#' @param method The output method type. One of "latex","html", or "console"
#'
#' @return vector of valid settings
#'
#' @name Valid
#' @aliases valid_colors  valid_style
#' @usage
#' valid_colors
#' valid_style
#'
NULL

#' Return vector of valid text color options
#' @rdname Valid
#' @export
valid_colors <- function(method = c("latex","html","console")){
  method <- match.arg(method,several.ok = TRUE)
  method <- match.arg(method,several.ok = TRUE)
  styles <- do.call(rbind, lapply(method, function(x) {
    styles <-
      eval(parse(text = paste0("color_key_", x)))[, c("Name", "hex")]
    colnames(styles) <- c("Color Name", "Hex Code")
    styles$source <- x
    return(styles)
  }))
  if(length(method) == 1){
    styles$source <- NULL
  }
  styles
}


#' Return vector of valid text styling options
#' @rdname Valid
#' @export
valid_style <- function(method = c("latex","html","console")){
  method <- match.arg(method)
  eval(parse(text = paste0("names(",method,"_style_codes)")))
}
