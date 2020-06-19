

style_wrapper_html <- function(styling, type = c("text","style","background")){
  if (is.na(styling)){
    ""
  }else{
    type <- match.arg(type)
    styling <- tolower(styling)
    switch(
      type,
      "text" = html_text_styling(unify_colors(styling, type = "html")),
      "style" = html_decoration_styling(styling),
      "background" = html_background_styling(unify_colors(styling, type = "html"))
    )
  }
}

html_decoration_styling <- function(styling){
  if (!styling %in% names(html_style_codes)) {
    ""
  }else{
    codes <- html_style_codes[[styling]]
    paste0(names(codes),":",codes,";",collapse = "")
  }
}

html_text_styling <- function(color){
  codes <- try(find_html_codes(color),silent = TRUE)
  if (inherits(codes, "try-error")) {
    ""
  }else{
    paste0("color:",codes,";",collapse = "")
  }
}

html_background_styling <- function(color){
  codes <- try(find_html_codes(color),silent = TRUE)
  if (inherits(codes, "try-error")) {
    ""
  } else{
    paste0("background:",codes,";",collapse = "")
  }
}

valid_html_codes <- function(color) {
  valid <- FALSE
  if (grepl("^#", color) & grepl("^#[0-9A-F]{6}$", color, perl = TRUE)) {
    valid <- TRUE
  } else if (color %in% color_key_html$Name) {
    valid <- TRUE
  } else{
    valid <- FALSE
  }
  return(valid)
}


find_html_codes <- function(input){
  if (valid_html_codes(input)) {
    return(input)
  }else{
    u_inputs <- unique(input)
    output_codes <- vector("character", length(input))
    rgb_mat <- col2rgb(u_inputs)
    rgb_key <- do.call(rbind, color_key_html$RGB)
    for (input_i in 1:length(u_inputs)) {
      input_rgb <- rgb_mat[, input_i, drop = TRUE]
      output_codes[input == u_inputs[input_i]] <-
        color_key_html$Name[which_closest_color(input_rgb, rgb_key)]
    }

    output_codes
  }
}

html_style_codes <- list(
  bold = list("font-weight" = "bold"),
  italic = list("font-style" = "italic"),
  underline = list("text-decoration" = "underline"),
  inverse = list("-webkit-filter" = "invert(100%)",
                 "filter" = "invert(100%)"),
  hidden = list("visibility" = "hidden"),
  strikethrough = list("text-decoration" = "line-through"),
  outline = list("-webkit-text-stroke" = "1px black",
                       "text-shadow" = "0px 0px 0px #000")
)

#' return the tibble of valid web colors
#'
#' Source is from https://www.w3schools.com/colors/color_tryit.asp
#'
#' @importFrom tibble tribble
#'
color_key_html  <- tibble::tribble(
              ~Name,     ~hex,          ~RGB,
             "aliceblue", "#F0F8FF", c(red = 240, green = 248, blue = 255),
          "antiquewhite", "#FAEBD7", c(red = 250, green = 235, blue = 215),
                  "aqua", "#00FFFF",   c(red = 0, green = 255, blue = 255),
            "aquamarine", "#7FFFD4", c(red = 127, green = 255, blue = 212),
                 "azure", "#F0FFFF", c(red = 240, green = 255, blue = 255),
                 "beige", "#F5F5DC", c(red = 245, green = 245, blue = 220),
                "bisque", "#FFE4C4", c(red = 255, green = 228, blue = 196),
                 "black", "#000000",       c(red = 0, green = 0, blue = 0),
        "blanchedalmond", "#FFEBCD", c(red = 255, green = 235, blue = 205),
                  "blue", "#0000FF",     c(red = 0, green = 0, blue = 255),
            "blueviolet", "#8A2BE2",  c(red = 138, green = 43, blue = 226),
                 "brown", "#A52A2A",   c(red = 165, green = 42, blue = 42),
             "burlywood", "#DEB887", c(red = 222, green = 184, blue = 135),
             "cadetblue", "#5F9EA0",  c(red = 95, green = 158, blue = 160),
            "chartreuse", "#7FFF00",   c(red = 127, green = 255, blue = 0),
             "chocolate", "#D2691E",  c(red = 210, green = 105, blue = 30),
                 "coral", "#FF7F50",  c(red = 255, green = 127, blue = 80),
        "cornflowerblue", "#6495ED", c(red = 100, green = 149, blue = 237),
              "cornsilk", "#FFF8DC", c(red = 255, green = 248, blue = 220),
               "crimson", "#DC143C",   c(red = 220, green = 20, blue = 60),
                  "cyan", "#00FFFF",   c(red = 0, green = 255, blue = 255),
              "darkblue", "#00008B",     c(red = 0, green = 0, blue = 139),
              "darkcyan", "#008B8B",   c(red = 0, green = 139, blue = 139),
         "darkgoldenrod", "#B8860B",  c(red = 184, green = 134, blue = 11),
              "darkgray", "#A9A9A9", c(red = 169, green = 169, blue = 169),
              "darkgrey", "#A9A9A9", c(red = 169, green = 169, blue = 169),
             "darkgreen", "#006400",     c(red = 0, green = 100, blue = 0),
             "darkkhaki", "#BDB76B", c(red = 189, green = 183, blue = 107),
           "darkmagenta", "#8B008B",   c(red = 139, green = 0, blue = 139),
        "darkolivegreen", "#556B2F",   c(red = 85, green = 107, blue = 47),
            "darkorange", "#FF8C00",   c(red = 255, green = 140, blue = 0),
            "darkorchid", "#9932CC",  c(red = 153, green = 50, blue = 204),
               "darkred", "#8B0000",     c(red = 139, green = 0, blue = 0),
            "darksalmon", "#E9967A", c(red = 233, green = 150, blue = 122),
          "darkseagreen", "#8FBC8F", c(red = 143, green = 188, blue = 143),
         "darkslateblue", "#483D8B",   c(red = 72, green = 61, blue = 139),
         "darkslategray", "#2F4F4F",    c(red = 47, green = 79, blue = 79),
         "darkslategrey", "#2F4F4F",    c(red = 47, green = 79, blue = 79),
         "darkturquoise", "#00CED1",   c(red = 0, green = 206, blue = 209),
            "darkviolet", "#9400D3",   c(red = 148, green = 0, blue = 211),
              "deeppink", "#FF1493",  c(red = 255, green = 20, blue = 147),
           "deepskyblue", "#00BFFF",   c(red = 0, green = 191, blue = 255),
               "dimgray", "#696969", c(red = 105, green = 105, blue = 105),
               "dimgrey", "#696969", c(red = 105, green = 105, blue = 105),
            "dodgerblue", "#1E90FF",  c(red = 30, green = 144, blue = 255),
             "firebrick", "#B22222",   c(red = 178, green = 34, blue = 34),
           "floralwhite", "#FFFAF0", c(red = 255, green = 250, blue = 240),
           "forestgreen", "#228B22",   c(red = 34, green = 139, blue = 34),
               "fuchsia", "#FF00FF",   c(red = 255, green = 0, blue = 255),
             "gainsboro", "#DCDCDC", c(red = 220, green = 220, blue = 220),
            "ghostwhite", "#F8F8FF", c(red = 248, green = 248, blue = 255),
                  "gold", "#FFD700",   c(red = 255, green = 215, blue = 0),
             "goldenrod", "#DAA520",  c(red = 218, green = 165, blue = 32),
                  "gray", "#808080", c(red = 128, green = 128, blue = 128),
                  "grey", "#808080", c(red = 128, green = 128, blue = 128),
                 "green", "#008000",     c(red = 0, green = 128, blue = 0),
           "greenyellow", "#ADFF2F",  c(red = 173, green = 255, blue = 47),
              "honeydew", "#F0FFF0", c(red = 240, green = 255, blue = 240),
               "hotpink", "#FF69B4", c(red = 255, green = 105, blue = 180),
             "indianred", "#CD5C5C",   c(red = 205, green = 92, blue = 92),
                "indigo", "#4B0082",    c(red = 75, green = 0, blue = 130),
                 "ivory", "#FFFFF0", c(red = 255, green = 255, blue = 240),
                 "khaki", "#F0E68C", c(red = 240, green = 230, blue = 140),
              "lavender", "#E6E6FA", c(red = 230, green = 230, blue = 250),
         "lavenderblush", "#FFF0F5", c(red = 255, green = 240, blue = 245),
             "lawngreen", "#7CFC00",   c(red = 124, green = 252, blue = 0),
          "lemonchiffon", "#FFFACD", c(red = 255, green = 250, blue = 205),
             "lightblue", "#ADD8E6", c(red = 173, green = 216, blue = 230),
            "lightcoral", "#F08080", c(red = 240, green = 128, blue = 128),
             "lighcyan", "#E0FFFF", c(red = 224, green = 255, blue = 255),
  "lightgoldenrodyellow", "#FAFAD2", c(red = 250, green = 250, blue = 210),
             "lightgray", "#D3D3D3", c(red = 211, green = 211, blue = 211),
             "lightgrey", "#D3D3D3", c(red = 211, green = 211, blue = 211),
            "lightgreen", "#90EE90", c(red = 144, green = 238, blue = 144),
             "lightpink", "#FFB6C1", c(red = 255, green = 182, blue = 193),
           "lightsalmon", "#FFA07A", c(red = 255, green = 160, blue = 122),
         "lightseagreen", "#20B2AA",  c(red = 32, green = 178, blue = 170),
          "lightskyblue", "#87CEFA", c(red = 135, green = 206, blue = 250),
        "lightslategray", "#778899", c(red = 119, green = 136, blue = 153),
        "lightslategrey", "#778899", c(red = 119, green = 136, blue = 153),
        "lightsteelblue", "#B0C4DE", c(red = 176, green = 196, blue = 222),
           "lightyellow", "#FFFFE0", c(red = 255, green = 255, blue = 224),
                  "lime", "#00FF00",     c(red = 0, green = 255, blue = 0),
             "limegreen", "#32CD32",   c(red = 50, green = 205, blue = 50),
                 "linen", "#FAF0E6", c(red = 250, green = 240, blue = 230),
               "magenta", "#FF00FF",   c(red = 255, green = 0, blue = 255),
                "maroon", "#800000",     c(red = 128, green = 0, blue = 0),
      "mediumaquamarine", "#66CDAA", c(red = 102, green = 205, blue = 170),
            "mediumblue", "#0000CD",     c(red = 0, green = 0, blue = 205),
          "mediumorchid", "#BA55D3",  c(red = 186, green = 85, blue = 211),
          "mediumpurple", "#9370DB", c(red = 147, green = 112, blue = 219),
        "mediumseagreen", "#3CB371",  c(red = 60, green = 179, blue = 113),
       "mediumslateblue", "#7B68EE", c(red = 123, green = 104, blue = 238),
     "mediumspringgreen", "#00FA9A",   c(red = 0, green = 250, blue = 154),
       "mediumturquoise", "#48D1CC",  c(red = 72, green = 209, blue = 204),
       "mediumvioletred", "#C71585",  c(red = 199, green = 21, blue = 133),
          "midnightblue", "#191970",   c(red = 25, green = 25, blue = 112),
             "mintcream", "#F5FFFA", c(red = 245, green = 255, blue = 250),
             "mistyrose", "#FFE4E1", c(red = 255, green = 228, blue = 225),
              "moccasin", "#FFE4B5", c(red = 255, green = 228, blue = 181),
           "navajowhite", "#FFDEAD", c(red = 255, green = 222, blue = 173),
                  "navy", "#000080",     c(red = 0, green = 0, blue = 128),
               "oldlace", "#FDF5E6", c(red = 253, green = 245, blue = 230),
                 "olive", "#808000",   c(red = 128, green = 128, blue = 0),
             "olivedrab", "#6B8E23",  c(red = 107, green = 142, blue = 35),
                "orange", "#FFA500",   c(red = 255, green = 165, blue = 0),
             "orangered", "#FF4500",    c(red = 255, green = 69, blue = 0),
                "orchid", "#DA70D6", c(red = 218, green = 112, blue = 214),
         "palegoldenrod", "#EEE8AA", c(red = 238, green = 232, blue = 170),
             "palegreen", "#98FB98", c(red = 152, green = 251, blue = 152),
         "palerturquoise", "#AFEEEE", c(red = 175, green = 238, blue = 238),
         "palevioletred", "#DB7093", c(red = 219, green = 112, blue = 147),
            "papayawhip", "#FFEFD5", c(red = 255, green = 239, blue = 213),
             "peachpuff", "#FFDAB9", c(red = 255, green = 218, blue = 185),
                  "peru", "#CD853F",  c(red = 205, green = 133, blue = 63),
                  "pink", "#FFC0CB", c(red = 255, green = 192, blue = 203),
                  "plum", "#DDA0DD", c(red = 221, green = 160, blue = 221),
            "powderblue", "#B0E0E6", c(red = 176, green = 224, blue = 230),
                "purple", "#800080",   c(red = 128, green = 0, blue = 128),
         "rebeccapurple", "#663399",  c(red = 102, green = 51, blue = 153),
                   "red", "#FF0000",     c(red = 255, green = 0, blue = 0),
             "rosybrown", "#BC8F8F", c(red = 188, green = 143, blue = 143),
             "royalblue", "#4169E1",  c(red = 65, green = 105, blue = 225),
           "saddlebrown", "#8B4513",   c(red = 139, green = 69, blue = 19),
                "salmon", "#FA8072", c(red = 250, green = 128, blue = 114),
            "sandybrown", "#F4A460",  c(red = 244, green = 164, blue = 96),
              "seagreen", "#2E8B57",   c(red = 46, green = 139, blue = 87),
              "seashell", "#FFF5EE", c(red = 255, green = 245, blue = 238),
                "sienna", "#A0522D",   c(red = 160, green = 82, blue = 45),
                "silver", "#C0C0C0", c(red = 192, green = 192, blue = 192),
               "skyblue", "#87CEEB", c(red = 135, green = 206, blue = 235),
             "slateblue", "#6A5ACD",  c(red = 106, green = 90, blue = 205),
             "slategray", "#708090", c(red = 112, green = 128, blue = 144),
             "slategrey", "#708090", c(red = 112, green = 128, blue = 144),
                  "snow", "#FFFAFA", c(red = 255, green = 250, blue = 250),
           "springgreen", "#00FF7F",   c(red = 0, green = 255, blue = 127),
             "steelblue", "#4682B4",  c(red = 70, green = 130, blue = 180),
                   "tan", "#D2B48C", c(red = 210, green = 180, blue = 140),
                  "teal", "#008080",   c(red = 0, green = 128, blue = 128),
               "thistle", "#D8BFD8", c(red = 216, green = 191, blue = 216),
                "tomato", "#FF6347",   c(red = 255, green = 99, blue = 71),
             "turquoise", "#40E0D0",  c(red = 64, green = 224, blue = 208),
                "violet", "#EE82EE", c(red = 238, green = 130, blue = 238),
                 "wheat", "#F5DEB3", c(red = 245, green = 222, blue = 179),
                 "white", "#FFFFFF", c(red = 255, green = 255, blue = 255),
            "whitesmoke", "#F5F5F5", c(red = 245, green = 245, blue = 245),
                "yellow", "#FFFF00",   c(red = 255, green = 255, blue = 0),
           "yellowgreen", "#9ACD32",  c(red = 154, green = 205, blue = 50)
  )

