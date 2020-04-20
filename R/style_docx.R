#' @importFrom rmarkdown latex_dependency
style_wrapper_docx <-
  function(styling,
           type = c("text", "style", "background")) {
    if (is.na(styling)) {
      ""
    } else{
      type <- match.arg(type)
      styling <- tolower(styling)
      switch(
        type,
        "style" = docx_decoration_styling(unify_colors(styling,type = NULL)),
        "text"  = docx_text_styling(styling),
        "background" = docx_background_styling(styling)
      )
    }
  }

style_zipper_docx <- function(x,styling){
  paste0("`<w:r><w:rPr xmlns:w14=\"http://schemas.microsoft.com/office/word/2010/wordml\">", styling, "</w:rPr><w:t xml:space=\"preserve\">",x,"</w:t></w:r>`{=openxml}")
}

docx_decoration_styling <- function(styling){
  if (!styling %in% names(docx_style_codes)) {
    ""
  }else{
    codes <- docx_style_codes[[styling]]
    paste0("<w: ",codes$code,"/>")
  }
}

docx_style_codes <- list(
  bold = list(code = "b"),
  italic = list(code = "i"),
  underline = list(code = "u"),
  strikethrough = list(code = "strike")
)

docx_text_styling <- function(color){
  code <- try(as_hex_codes(color), silent = TRUE)
  if (inherits(code, "try-error")) {
    ""
  } else{
    paste0("<w:color w:val=\"",code,"\"/>")
  }
}

docx_background_styling <- function(color){
  code <- try(as_docx_highlighter(color), silent = TRUE)
  if(inherits(code, "try-error")){
    ""
  }else{
    paste0("<w:highlight w:val=\"",code,"\"/>")
  }
}

#' @importFrom grDevices col2rgb
as_hex_codes <- function(x) {
  if (grepl("^(#)", x) |
      grepl("^(#)*[0-9A-Fa-f]{6}$", x, perl = TRUE)) {
    x <- gsub("^#", "", x) # remove initial hex pound
    return(toupper(x))
  } else {
    colors <- valid_colors()
    method_colors <- colors$`Color Name`
    if (x %in% method_colors) {
      idx <- which(colors$`Color Name` %in% x)
      if (length(idx) > 0) {
        return(colors$`Hex Code`[min(idx)])
      }
    } else {
      stop(
        paste(
          "Invalid Color Name being used. check for valid color names using `valid_colors()`"
        )
      )
    }
  }
}


as_docx_highlighter <- function(color){
  if(color %in% color_key_docx_highlighter$Name){
    idx <- which(color_key_docx_highlighter$Name == color)
  }else{
    rgb_mat <- col2rgb(as_hex_codes(color))[,1, drop = TRUE]
    rgb_key <- do.call('rbind',color_key_docx_highlighter$RGB)
    idx <- which_closest_color(rgb_mat, rgb_key)
  }
  c(color_key_docx_highlighter[idx,"code", drop = TRUE])
}


color_key_docx_highlighter <- tibble::tribble(
  ~Name,     ~hex,  ~code, ~RGB,
  "yellow","#ffff00","yellow",c(red = 255, blue = 0, green = 255),
  "lightgreen","#00ff00","green",c(red = 0, blue = 0, green = 255),
  "cyan","#00ffff","cyan",c(red = 0, blue = 255, green = 255),
  "pink","#ff00ff","magenta",c(red = 255, blue = 255, green = 0),
  "blue","#0000ff","blue",c(red = 0, blue = 255, green = 0),
  "red","#ff0000","red",c(red = 255, blue = 0, green = 0),
  "darkblue","#000080","darkBlue",c(red = 0, blue = 128, green = 0),
  "teal","#008080","darkCyan",c(red = 0, blue = 128, green = 128),
  "green","#008000","darkGreen",c(red = 0, blue = 0, green = 128),
  "violet","#800080","darkMagenta",c(red = 128, blue = 128, green = 0),
  "darkred","#800000","darkRed",c(red = 128, blue = 0, green = 0),
  "darkyellow","#808000","darkYellow",c(red = 128, blue = 0, green = 128),
  "darkgray","#808080","darkGray",c(red = 128, blue = 128, green = 128),
  "lightgray","#c0c0c0","lightGray",c(red = 192, blue = 192, green = 192),
  "black","#000000","black",c(red = 0, blue = 0, green = 0)
)




