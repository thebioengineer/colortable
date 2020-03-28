
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
  if (!styling %in% names(console_style_codes)) {
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
  codes <- try(find_acsii_codes(color),silent = TRUE)
  if(inherits(codes,"try-error")){
    function(x){x}
  }else{
  function(x){
    paste0(paste0('\033[38;5;', codes, 'm', collapse = ""),
           x,
           "\033[0m")
  }}
}

console_background_styling <- function(color){
  codes <- try(find_acsii_codes(color),silent = TRUE)
  if(inherits(codes,"try-error")){
    function(x){x}
  }else{
  function(x){
    paste0(paste0('\033[48;5;', codes, 'm', collapse = ""),
           x,
           "\033[0m")
  }}
}

find_acsii_codes <- function(input){

    u_inputs <- unique(input)
    output_codes <- vector("character", length(input))
    rgb_mat <- col2rgb(u_inputs)
    rgb_key <- do.call(rbind, color_key$RGB)

    for (input_i in 1:length(u_inputs)) {
      input_rgb <- rgb_mat[, input_i, drop = TRUE]
      output_codes[input == u_inputs[input_i]] <-
        color_key$`Xterm Number`[which_closest_color(input_rgb, rgb_key)]
    }
    output_codes
}

# Based on https://stackoverflow.com/questions/1847092/given-an-rgb-value-what-would-be-the-best-way-to-find-the-closest-match-in-the-dhttps://stackoverflow.com/questions/1847092/given-an-rgb-value-what-would-be-the-best-way-to-find-the-closest-match-in-the-d
which_closest_color <- function(to_match, rgb_vect){
  method <- tolower(getOption("colortable.color_approx.method",default = "euclidian"))
  func <- switch(method,
         euclidian = which_closest_color.euclidian,
         weighted = which_closest_color.weighted
  )
  func(to_match, rgb_vect)
}

which_closest_color.euclidian <- function(to_match, rgb_vect){
  which.min(sqrt(
    ((rgb_vect[, 1] - to_match[[1]]))  ^ 2 + # red
      ((rgb_vect[, 2] - to_match[[2]])) ^ 2 + # green
      ((rgb_vect[, 3] - to_match[[3]])) ^ 2   # blue
  ))
}

# https://www.compuphase.com/cmetric.htm
which_closest_color.weighted <- function(to_match, rgb_vect){
  r_bar <- rgb_vect[, 1] + to_match[[1]] / 2

  delta_c <- sqrt(
     ((2 + (r_bar/256)) * ((rgb_vect[, 1] - to_match[[1]]) ^ 2)) + # red
     (4 * ((rgb_vect[, 2] - to_match[[2]]) ^ 2)) + # green
     ((2 + (255 - r_bar)/256) * ((rgb_vect[,3] - to_match[[3]]) ^ 2)) # blue
  )
  which.min(delta_c)
}
