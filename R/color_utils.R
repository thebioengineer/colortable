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

  text_style <- style_wrapper_console(style, type = "style")
  text_color <- style_wrapper_console(text_color, type = "text")
  text_background <- style_wrapper_console(background, type = "background")
  class(x)<- setdiff(class(x),"colortable_cell")

  text_color(
    text_background(
      text_style(
       format(x,...)
       )))
}

style2html <- function(x, style = NA, text_color = NA, background = NA, ...){

  text_style <- style_wrapper_html(style, type = "style")
  text_color <- style_wrapper_html(text_color, type = "text")
  text_background <- style_wrapper_html(background, type = "background")
  class(x)<- setdiff(class(x),"colortable_cell")

  style = paste(c(text_style,text_color,text_background),collapse ="")
  paste0("<span style='",style,"'>",format(x,...),"</span>")
}

style2tex <- function(x, style = NA, text_color = NA, background = NA, ...){

  text_style <- style_wrapper_tex(style, type = "style")
  text_color <- style_wrapper_tex(text_color, type = "text")
  text_background <- style_wrapper_tex(background, type = "background")
  class(x)<- setdiff(class(x),"colortable_cell")

  text_background(text_style(text_color(format(x,...))))
}

paste_quote <- function(x, quote = TRUE){
  if(quote){
    paste0("\"",gsub("\"","\\\\\"",x),"\"")
  }else{
    x
  }
}

## Copied from the crayon package.
console_style_codes <- list(
  bold = list(1, 22), # 21 isn't widely supported and 22 does the same thing
  italic = list(3, 23),
  underline = list(4, 24),
  inverse = list(7, 27),
  hidden = list(8, 28),
  strikethrough = list(9, 29)
)

style_wrapper_console <-function(styling, type = c("text", "style", "background")) {
    if (is.na(styling)) {
      function(x) {
        x
      }
    } else{
      type <- match.arg(type)
      switch(
        type,
        "text" = console_text_styling(styling),
        "style" = console_decoration_styling(styling),
        "background" = console_background_styling(styling)
      )
    }
  }

console_decoration_styling <- function(styling){
  if (!styling %in% names(latex_style_codes)) {
    warning("Text decoration",
    " output by '",
    styling,
    "' has not been implemented for console output.",
    call. = FALSE)
    function(x){x}
  }else{
    codes <- console_style_codes[[styling]]
    function(x){
      paste0(paste0('\u001b[', codes[[1]], 'm', collapse=""),
             x,
             paste0('\u001b[', codes[[2]][1], 'm', collapse=""))
    }
  }
}

console_text_styling <- function(color){
  color <- tolower(color)
  if (!color %in% color_key$Name) {
    warning("Text Color",
            " output by '",
            color,
            "' has not been implemented for console output.",
            call. = FALSE)
    function(x){x}
  }else{
    codes <- color_key$`Xterm Number`[color_key$Name == color]
    function(x){
      paste0(paste0('\033[38;5;', codes, 'm', collapse = ""),
             x,
             "\033[0m")
    }
  }
}

console_background_styling <- function(color){
  color <- tolower(color)
  if (!color %in% color_key$Name) {
    warning("Background Color",
            " output by '",
            color,
            "' has not been implemented for console output.",
            call. = FALSE)
    function(x){x}
  }else{
    codes <- color_key$`Xterm Number`[color_key$Name == color]
    function(x){
      paste0(paste0('\033[48;5;', codes, 'm', collapse = ""),
             x,
             "\033[0m")
    }
  }
}
## based on crayon package
html_style_codes <- list(

  style.bold = list("font-weight"="bold"),
  style.italic = list("font-style"="italic"),
  style.underline = list("text-decoration"="underline"),
  style.inverse = list("-webkit-filter"="invert(100%)",
                       "filter"="invert(100%)"),
  style.hidden = list("visibility"="hidden"),
  style.strikethrough = list("text-decoration"= "line-through"),

  text.black = list("color"="black"),
  text.red = list("color"="red"),
  text.green = list("color"="green"),
  text.yellow = list("color"="yellow"),
  text.blue = list("color"="blue"),
  text.magenta = list("color"="magenta"),
  text.cyan = list("color"="cyan"),
  text.white = list("color"="white"),
  text.silver = list("color"="silver"),

  bg.black = list("background"="black"),
  bg.red = list("background"="red"),
  bg.green = list("background"="green"),
  bg.yellow = list("background"="yellow"),
  bg.blue = list("background"="blue"),
  bg.magenta = list("background"="magenta"),
  bg.cyan = list("background"="cyan"),
  bg.white = list("background"="white")
)

style_wrapper_html <- function(styling, type = c("text","style","background")){
  if(is.na(styling)){
    ""
  }else{
    type <- match.arg(type)
    styling2 <- switch(type,
                      "text" = paste0("text.",styling),
                      "style" = paste0("style.",styling),
                      "background" = paste0("bg.",styling))

    if(!styling2 %in% names(latex_style_codes)){
      warning(switch(
        type,
        text = "Text coloring",
        style = "Text styling",
        background = "Background coloring"
      ),
      " output by '",
      styling,
      "' has not been implemented for console output.",
      call. = FALSE)
      ""
    }else{
      stopifnot(styling2 %in% names(html_style_codes))
      codes <- html_style_codes[[styling2]]

      paste0(names(codes),":",codes,";",collapse = "")
    }
  }
}

## based on crayon package
latex_style_codes <- list(

  style.bold = list("\\textbf"=""),
  style.italic = list("\\textbf"=""),
  style.underline = list("\\underline"=""),
  style.strikethrough = list("\\sout"=""),

  text.black = list("\\textcolor"="black"),
  text.red = list("\\textcolor"="red"),
  text.green = list("\\textcolor"="green"),
  text.yellow = list("\\textcolor{"="yellow"),
  text.blue = list("\\textcolor"="blue"),
  text.magenta = list("\\textcolor"="magenta"),
  text.cyan = list("\\textcolor"="cyan"),
  text.white = list("\\textcolor"="white"),
  text.silver = list("\\textcolor"="silver"),

  bg.black = list("\\colorbox"="black"),
  bg.red = list("\\colorbox"="red"),
  bg.green = list("\\colorbox"="green"),
  bg.yellow = list("\\colorbox"="yellow"),
  bg.blue = list("\\colorbox"="blue"),
  bg.magenta = list("\\colorbox"="magenta"),
  bg.cyan = list("\\colorbox"="cyan"),
  bg.white = list("\\colorbox"="white")
)

#' @importFrom rmarkdown latex_dependency
#' @importFrom knitr knit_meta_add
style_wrapper_tex <- function(styling, type = c("text","style","background")){
  if(is.na(styling)){
    function(x){x}
  }else{
    type <- match.arg(type)
    styling2 <- switch(type,
                      "text" = paste0("text.",styling),
                      "style" = paste0("style.",styling),
                      "background" = paste0("bg.",styling))

    if(!styling2 %in% names(latex_style_codes)){

      warning(switch(
        type,
        text = "Text coloring",
        style = "Text styling",
        background = "Background coloring"
      ),
      " output by '",
      styling,
      "' has not been implemented for console output.",
      call. = FALSE)

      function(x){x}
    }else{

      stopifnot(styling2 %in% names(latex_style_codes))
      codes <- latex_style_codes[[styling2]]

      if(styling2 == "style.strikethrough"){
        knitr::knit_meta_add(list(rmarkdown::latex_dependency("ulem")))
      }

      function(x){
        wrapper <- paste0(names(codes),"{")
        if(codes != ""){
          wrapper <- paste0(wrapper,codes,"}{")
        }
        paste0(wrapper,x,"}")
      }
    }
  }
}

