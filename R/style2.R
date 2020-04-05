#' Append Coloring or Styling
#'
#' Based on inputs, determines correct strings to append to the input to style
#' for any type of output (console, html, pdf, ...)
#'
#' @param x a vector of length one
#' @param style styling to append to `x`
#' @param text_color text color to set `x`
#' @param background background color to set `x`
#' @param ... arguments to pass to `format()`
#'
#' @return a character vector with styling appended
#' @examples
#' console_text <- colortable:::style2console(24, text_color = "red" )
#' html_text <- colortable:::style2html(24, text_color = "red" )
#' tex_text <- colortable:::style2tex(24, text_color = "red" )
#'
#' cat(console_text)
#'
#' @usage
#' style2console
#' style2html
#' style2tex
NULL

style2console <- function(x, style = NA, text_color = NA, background = NA, ...){
  if (is.na(x)) {
    return(NA)
  }else{
    text_style <- style_wrapper_console(style, type = "style")
    text_color <- style_wrapper_console(text_color, type = "text")
    text_background <- style_wrapper_console(background, type = "background")

    text_color(text_background(text_style(x)))
  }
}

style2consoleV <-
  Vectorize(
    style2console,
    vectorize.args = c("x", "style", "text_color", "background"),
    SIMPLIFY = TRUE
  )

style2html <- function(x, style = NA, text_color = NA, background = NA, ...){
  if (is.na(x)) {
    return(NA)
  }else{
    text_style <- style_wrapper_html(style, type = "style")
    text_color <- style_wrapper_html(text_color, type = "text")
    text_background <- style_wrapper_html(background, type = "background")

    style = paste(c(text_style, text_color, text_background), collapse =
                    "")
    paste0("<span style='",style,"'>",x,"</span>")
  }
}

style2htmlV <- Vectorize(style2html,vectorize.args = c("x","style","text_color","background"),SIMPLIFY = TRUE)

style2tex <- function(x, style = NA, text_color = NA, background = NA, ...){
  if (is.na(x)) {
    return(NA)
  }else{
    text_style <- style_wrapper_tex(style, type = "style")
    text_color <- style_wrapper_tex(text_color, type = "text")
    text_background <- style_wrapper_tex(background, type = "background")

    text_background(text_style(text_color(x)))
  }
}

style2texV <- Vectorize(style2tex,vectorize.args = c("x","style","text_color","background"),SIMPLIFY = TRUE)
