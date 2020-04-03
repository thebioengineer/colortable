
style_wrapper_console <-function(styling, type = c("text", "style", "background")) {
  if (is.na(styling)) {
    function(x) {
      x
    }
  } else{
    type <- match.arg(type)
    styling <- tolower(styling)
    switch(
      type,
      "text" = console_text_styling(unify_colors(styling, type = "console")),
      "style" = console_decoration_styling(styling),
      "background" = console_background_styling(unify_colors(styling, type = "console"))
    )
  }
}

console_decoration_styling <- function(styling){
  if (!styling %in% names(console_style_codes)) {
    function(x) {
      x
    }
  } else{
    codes <- console_style_codes[[styling]]
    function(x) {
      paste0(
        paste0('\u001b[', codes[[1]], 'm', collapse = ""),
        x,
        paste0('\u001b[', codes[[2]][1], 'm', collapse = "")
      )
    }
  }
}

console_text_styling <- function(color){
  codes <- try(find_acsii_codes(color), silent = TRUE)
  if (inherits(codes, "try-error")) {
    function(x) {
      x
    }
  } else{
    function(x) {
      paste0(paste0('\033[38;5;', codes, 'm', collapse = ""),
             x,
             "\033[0m")
    }
  }
}

console_background_styling <- function(color){
  codes <- try(find_acsii_codes(color),silent = TRUE)
  if (inherits(codes, "try-error")) {
    function(x) {
      x
    }
  } else{
    function(x) {
      paste0(paste0('\033[48;5;', codes, 'm', collapse = ""),
             x,
             "\033[0m")
    }
  }
}

find_acsii_codes <- function(input){

    u_inputs <- unique(input)
    output_codes <- vector("character", length(input))
    rgb_mat <- col2rgb(u_inputs)
    rgb_key <- do.call(rbind, color_key_console$RGB)

    for (input_i in 1:length(u_inputs)) {
      input_rgb <- rgb_mat[, input_i, drop = TRUE]
      output_codes[input == u_inputs[input_i]] <-
        color_key_console$`Xterm Number`[which_closest_color(input_rgb, rgb_key)]
    }
    output_codes
}


console_style_codes <- list(
  bold = list(1, 22), # 21 isn't widely supported and 22 does the same thing
  italic = list(3, 23),
  underline = list(4, 24),
  inverse = list(7, 27),
  hidden = list(8, 28),
  strikethrough = list(9, 29)
)

#' return the tibble of color keys
#'
#' Source is from https://jonasjacek.github.io/colors/
#'
#' @importFrom tibble tribble
#'
color_key_console <- tribble(
  ~Name, ~`Xterm Number`, ~ hex, ~RGB,
  "black","0","#000000",c(red = 0, green = 0, blue = 0),
  "maroon","1","#800000",c(red = 128, green = 0, blue = 0),
  "green","2","#008000",c(red = 0, green = 128, blue = 0),
  "olive","3","#808000",c(red = 128, green = 128, blue = 0),
  "navy","4","#000080",c(red = 0, green = 0, blue = 128),
  "purple","5","#800080",c(red = 128, green = 0, blue = 128),
  "teal","6","#008080",c(red = 0, green = 128, blue = 128),
  "silver","7","#c0c0c0",c(red = 192, green = 192, blue = 192),
  "grey","8","#808080",c(red = 128, green = 128, blue = 128),
  "red","9","#ff0000",c(red = 255, green = 0, blue = 0),
  "lime","10","#00ff00",c(red = 0, green = 255, blue = 0),
  "yellow","11","#ffff00",c(red = 255, green = 255, blue = 0),
  "blue","12","#0000ff",c(red = 0, green = 0, blue = 255),
  "fuchsia","13","#ff00ff",c(red = 255, green = 0, blue = 255),
  "aqua","14","#00ffff",c(red = 0, green = 255, blue = 255),
  "white","15","#ffffff",c(red = 255, green = 255, blue = 255),
  "grey0","16","#000000",c(red = 0, green = 0, blue = 0),
  "navyblue","17","#00005f",c(red = 0, green = 0, blue = 95),
  "darkblue","18","#000087",c(red = 0, green = 0, blue = 135),
  "blue3","19","#0000af",c(red = 0, green = 0, blue = 175),
  "blue2","20","#0000d7",c(red = 0, green = 0, blue = 215),
  "blue1","21","#0000ff",c(red = 0, green = 0, blue = 255),
  "darkgreen","22","#005f00",c(red = 0, green = 95, blue = 0),
  "deepskyblue7","23","#005f5f",c(red = 0, green = 95, blue = 95),
  "deepskyblue6","24","#005f87",c(red = 0, green = 95, blue = 135),
  "deepskyblue5","25","#005faf",c(red = 0, green = 95, blue = 175),
  "dodgerblue3","26","#005fd7",c(red = 0, green = 95, blue = 215),
  "dodgerblue2","27","#005fff",c(red = 0, green = 95, blue = 255),
  "green4","28","#008700",c(red = 0, green = 135, blue = 0),
  "springgreen6","29","#00875f",c(red = 0, green = 135, blue = 95),
  "turquoise4","30","#008787",c(red = 0, green = 135, blue = 135),
  "deepskyblue4","31","#0087af",c(red = 0, green = 135, blue = 175),
  "deepskyblue3","32","#0087d7",c(red = 0, green = 135, blue = 215),
  "dodgerblue1","33","#0087ff",c(red = 0, green = 135, blue = 255),
  "green3","34","#00af00",c(red = 0, green = 175, blue = 0),
  "springgreen5","35","#00af5f",c(red = 0, green = 175, blue = 95),
  "darkcyan","36","#00af87",c(red = 0, green = 175, blue = 135),
  "lightseagreen","37","#00afaf",c(red = 0, green = 175, blue = 175),
  "deepskyblue2","38","#00afd7",c(red = 0, green = 175, blue = 215),
  "deepskyblue1","39","#00afff",c(red = 0, green = 175, blue = 255),
  "green2","40","#00d700",c(red = 0, green = 215, blue = 0),
  "springgreen4","41","#00d75f",c(red = 0, green = 215, blue = 95),
  "springgreen3","42","#00d787",c(red = 0, green = 215, blue = 135),
  "cyan3","43","#00d7af",c(red = 0, green = 215, blue = 175),
  "darkturquoise","44","#00d7d7",c(red = 0, green = 215, blue = 215),
  "turquoise2","45","#00d7ff",c(red = 0, green = 215, blue = 255),
  "green1","46","#00ff00",c(red = 0, green = 255, blue = 0),
  "springgreen2","47","#00ff5f",c(red = 0, green = 255, blue = 95),
  "springgreen1","48","#00ff87",c(red = 0, green = 255, blue = 135),
  "mediumspringgreen","49","#00ffaf",c(red = 0, green = 255, blue = 175),
  "cyan2","50","#00ffd7",c(red = 0, green = 255, blue = 215),
  "cyan1","51","#00ffff",c(red = 0, green = 255, blue = 255),
  "darkred","52","#5f0000",c(red = 95, green = 0, blue = 0),
  "deeppink8","53","#5f005f",c(red = 95, green = 0, blue = 95),
  "purple6","54","#5f0087",c(red = 95, green = 0, blue = 135),
  "purple5","55","#5f00af",c(red = 95, green = 0, blue = 175),
  "purple4","56","#5f00d7",c(red = 95, green = 0, blue = 215),
  "blueviolet","57","#5f00ff",c(red = 95, green = 0, blue = 255),
  "orange4","58","#5f5f00",c(red = 95, green = 95, blue = 0),
  "grey37","59","#5f5f5f",c(red = 95, green = 95, blue = 95),
  "mediumpurple6","60","#5f5f87",c(red = 95, green = 95, blue = 135),
  "slateblue3","61","#5f5faf",c(red = 95, green = 95, blue = 175),
  "slateblue2","62","#5f5fd7",c(red = 95, green = 95, blue = 215),
  "royalblue1","63","#5f5fff",c(red = 95, green = 95, blue = 255),
  "chartreuse6","64","#5f8700",c(red = 95, green = 135, blue = 0),
  "darkseagreen8","65","#5f875f",c(red = 95, green = 135, blue = 95),
  "paleturquoise4","66","#5f8787",c(red = 95, green = 135, blue = 135),
  "steelblue","67","#5f87af",c(red = 95, green = 135, blue = 175),
  "steelblue3","68","#5f87d7",c(red = 95, green = 135, blue = 215),
  "cornflowerblue","69","#5f87ff",c(red = 95, green = 135, blue = 255),
  "chartreuse5","70","#5faf00",c(red = 95, green = 175, blue = 0),
  "darkseagreen7","71","#5faf5f",c(red = 95, green = 175, blue = 95),
  "cadetblue2","72","#5faf87",c(red = 95, green = 175, blue = 135),
  "cadetblue","73","#5fafaf",c(red = 95, green = 175, blue = 175),
  "skyblue3","74","#5fafd7",c(red = 95, green = 175, blue = 215),
  "steelblue2","75","#5fafff",c(red = 95, green = 175, blue = 255),
  "chartreuse4","76","#5fd700",c(red = 95, green = 215, blue = 0),
  "palegreen4","77","#5fd75f",c(red = 95, green = 215, blue = 95),
  "seagreen4","78","#5fd787",c(red = 95, green = 215, blue = 135),
  "aquamarine3","79","#5fd7af",c(red = 95, green = 215, blue = 175),
  "mediumturquoise","80","#5fd7d7",c(red = 95, green = 215, blue = 215),
  "steelblue1","81","#5fd7ff",c(red = 95, green = 215, blue = 255),
  "chartreuse3","82","#5fff00",c(red = 95, green = 255, blue = 0),
  "seagreen3","83","#5fff5f",c(red = 95, green = 255, blue = 95),
  "seagreen2","84","#5fff87",c(red = 95, green = 255, blue = 135),
  "seagreen1","85","#5fffaf",c(red = 95, green = 255, blue = 175),
  "aquamarine2","86","#5fffd7",c(red = 95, green = 255, blue = 215),
  "darkslategray2","87","#5fffff",c(red = 95, green = 255, blue = 255),
  "darkred2","88","#870000",c(red = 135, green = 0, blue = 0),
  "deeppink7","89","#87005f",c(red = 135, green = 0, blue = 95),
  "darkmagenta1","90","#870087",c(red = 135, green = 0, blue = 135),
  "darkmagenta","91","#8700af",c(red = 135, green = 0, blue = 175),
  "darkviolet1","92","#8700d7",c(red = 135, green = 0, blue = 215),
  "purple3","93","#8700ff",c(red = 135, green = 0, blue = 255),
  "orange3","94","#875f00",c(red = 135, green = 95, blue = 0),
  "lightpink4","95","#875f5f",c(red = 135, green = 95, blue = 95),
  "plum4","96","#875f87",c(red = 135, green = 95, blue = 135),
  "mediumpurple5","97","#875faf",c(red = 135, green = 95, blue = 175),
  "mediumpurple4","98","#875fd7",c(red = 135, green = 95, blue = 215),
  "slateblue1","99","#875fff",c(red = 135, green = 95, blue = 255),
  "yellow6","100","#878700",c(red = 135, green = 135, blue = 0),
  "wheat4","101","#87875f",c(red = 135, green = 135, blue = 95),
  "grey53","102","#878787",c(red = 135, green = 135, blue = 135),
  "lightslategrey","103","#8787af",c(red = 135, green = 135, blue = 175),
  "mediumpurple","104","#8787d7",c(red = 135, green = 135, blue = 215),
  "lightslateblue","105","#8787ff",c(red = 135, green = 135, blue = 255),
  "yellow5","106","#87af00",c(red = 135, green = 175, blue = 0),
  "darkolivegreen6","107","#87af5f",c(red = 135, green = 175, blue = 95),
  "darkseagreen","108","#87af87",c(red = 135, green = 175, blue = 135),
  "lightskyblue3","109","#87afaf",c(red = 135, green = 175, blue = 175),
  "lightskyblue2","110","#87afd7",c(red = 135, green = 175, blue = 215),
  "skyblue2","111","#87afff",c(red = 135, green = 175, blue = 255),
  "chartreuse2","112","#87d700",c(red = 135, green = 215, blue = 0),
  "darkolivegreen5","113","#87d75f",c(red = 135, green = 215, blue = 95),
  "palegreen3","114","#87d787",c(red = 135, green = 215, blue = 135),
  "darkseagreen6","115","#87d7af",c(red = 135, green = 215, blue = 175),
  "darkslategray3","116","#87d7d7",c(red = 135, green = 215, blue = 215),
  "skyblue1","117","#87d7ff",c(red = 135, green = 215, blue = 255),
  "chartreuse1","118","#87ff00",c(red = 135, green = 255, blue = 0),
  "lightgreen1","119","#87ff5f",c(red = 135, green = 255, blue = 95),
  "lightgreen","120","#87ff87",c(red = 135, green = 255, blue = 135),
  "palegreen2","121","#87ffaf",c(red = 135, green = 255, blue = 175),
  "aquamarine1","122","#87ffd7",c(red = 135, green = 255, blue = 215),
  "darkslategray1","123","#87ffff",c(red = 135, green = 255, blue = 255),
  "red3","124","#af0000",c(red = 175, green = 0, blue = 0),
  "deeppink6","125","#af005f",c(red = 175, green = 0, blue = 95),
  "mediumvioletred","126","#af0087",c(red = 175, green = 0, blue = 135),
  "magenta6","127","#af00af",c(red = 175, green = 0, blue = 175),
  "darkviolet","128","#af00d7",c(red = 175, green = 0, blue = 215),
  "purple2","129","#af00ff",c(red = 175, green = 0, blue = 255),
  "darkorange3","130","#af5f00",c(red = 175, green = 95, blue = 0),
  "indianred","131","#af5f5f",c(red = 175, green = 95, blue = 95),
  "hotpink4","132","#af5f87",c(red = 175, green = 95, blue = 135),
  "mediumorchid3","133","#af5faf",c(red = 175, green = 95, blue = 175),
  "mediumorchid","134","#af5fd7",c(red = 175, green = 95, blue = 215),
  "mediumpurple3","135","#af5fff",c(red = 175, green = 95, blue = 255),
  "darkgoldenrod","136","#af8700",c(red = 175, green = 135, blue = 0),
  "lightsalmon2","137","#af875f",c(red = 175, green = 135, blue = 95),
  "rosybrown","138","#af8787",c(red = 175, green = 135, blue = 135),
  "grey63","139","#af87af",c(red = 175, green = 135, blue = 175),
  "mediumpurple2","140","#af87d7",c(red = 175, green = 135, blue = 215),
  "mediumpurple1","141","#af87ff",c(red = 175, green = 135, blue = 255),
  "gold3","142","#afaf00",c(red = 175, green = 175, blue = 0),
  "darkkhaki","143","#afaf5f",c(red = 175, green = 175, blue = 95),
  "navajowhite3","144","#afaf87",c(red = 175, green = 175, blue = 135),
  "grey69","145","#afafaf",c(red = 175, green = 175, blue = 175),
  "lightsteelblue3","146","#afafd7",c(red = 175, green = 175, blue = 215),
  "lightsteelblue","147","#afafff",c(red = 175, green = 175, blue = 255),
  "yellow4","148","#afd700",c(red = 175, green = 215, blue = 0),
  "darkolivegreen4","149","#afd75f",c(red = 175, green = 215, blue = 95),
  "darkseagreen5","150","#afd787",c(red = 175, green = 215, blue = 135),
  "darkseagreen4","151","#afd7af",c(red = 175, green = 215, blue = 175),
  "lightcyan3","152","#afd7d7",c(red = 175, green = 215, blue = 215),
  "lightskyblue1","153","#afd7ff",c(red = 175, green = 215, blue = 255),
  "greenyellow","154","#afff00",c(red = 175, green = 255, blue = 0),
  "darkolivegreen3","155","#afff5f",c(red = 175, green = 255, blue = 95),
  "palegreen1","156","#afff87",c(red = 175, green = 255, blue = 135),
  "darkseagreen3","157","#afffaf",c(red = 175, green = 255, blue = 175),
  "darkseagreen2","158","#afffd7",c(red = 175, green = 255, blue = 215),
  "paleturquoise1","159","#afffff",c(red = 175, green = 255, blue = 255),
  "red2","160","#d70000",c(red = 215, green = 0, blue = 0),
  "deeppink5","161","#d7005f",c(red = 215, green = 0, blue = 95),
  "deeppink4","162","#d70087",c(red = 215, green = 0, blue = 135),
  "magenta5","163","#d700af",c(red = 215, green = 0, blue = 175),
  "magenta4","164","#d700d7",c(red = 215, green = 0, blue = 215),
  "magenta3","165","#d700ff",c(red = 215, green = 0, blue = 255),
  "darkorange1","166","#d75f00",c(red = 215, green = 95, blue = 0),
  "indianred2","167","#d75f5f",c(red = 215, green = 95, blue = 95),
  "hotpink3","168","#d75f87",c(red = 215, green = 95, blue = 135),
  "hotpink2","169","#d75faf",c(red = 215, green = 95, blue = 175),
  "orchid","170","#d75fd7",c(red = 215, green = 95, blue = 215),
  "mediumorchid2","171","#d75fff",c(red = 215, green = 95, blue = 255),
  "orange2","172","#d78700",c(red = 215, green = 135, blue = 0),
  "lightsalmon3","173","#d7875f",c(red = 215, green = 135, blue = 95),
  "lightpink3","174","#d78787",c(red = 215, green = 135, blue = 135),
  "pink3","175","#d787af",c(red = 215, green = 135, blue = 175),
  "plum3","176","#d787d7",c(red = 215, green = 135, blue = 215),
  "violet","177","#d787ff",c(red = 215, green = 135, blue = 255),
  "gold2","178","#d7af00",c(red = 215, green = 175, blue = 0),
  "lightgoldenrod5","179","#d7af5f",c(red = 215, green = 175, blue = 95),
  "tan","180","#d7af87",c(red = 215, green = 175, blue = 135),
  "mistyrose3","181","#d7afaf",c(red = 215, green = 175, blue = 175),
  "thistle3","182","#d7afd7",c(red = 215, green = 175, blue = 215),
  "plum2","183","#d7afff",c(red = 215, green = 175, blue = 255),
  "yellow3","184","#d7d700",c(red = 215, green = 215, blue = 0),
  "khaki3","185","#d7d75f",c(red = 215, green = 215, blue = 95),
  "lightgoldenrod4","186","#d7d787",c(red = 215, green = 215, blue = 135),
  "lightyellow3","187","#d7d7af",c(red = 215, green = 215, blue = 175),
  "grey84","188","#d7d7d7",c(red = 215, green = 215, blue = 215),
  "lightsteelblue1","189","#d7d7ff",c(red = 215, green = 215, blue = 255),
  "yellow2","190","#d7ff00",c(red = 215, green = 255, blue = 0),
  "darkolivegreen2","191","#d7ff5f",c(red = 215, green = 255, blue = 95),
  "darkolivegreen1","192","#d7ff87",c(red = 215, green = 255, blue = 135),
  "darkseagreen1","193","#d7ffaf",c(red = 215, green = 255, blue = 175),
  "honeydew2","194","#d7ffd7",c(red = 215, green = 255, blue = 215),
  "lightcyan1","195","#d7ffff",c(red = 215, green = 255, blue = 255),
  "red1","196","#ff0000",c(red = 255, green = 0, blue = 0),
  "deeppink3","197","#ff005f",c(red = 255, green = 0, blue = 95),
  "deeppink2","198","#ff0087",c(red = 255, green = 0, blue = 135),
  "deeppink1","199","#ff00af",c(red = 255, green = 0, blue = 175),
  "magenta2","200","#ff00d7",c(red = 255, green = 0, blue = 215),
  "magenta1","201","#ff00ff",c(red = 255, green = 0, blue = 255),
  "orangered1","202","#ff5f00",c(red = 255, green = 95, blue = 0),
  "indianred3","203","#ff5f5f",c(red = 255, green = 95, blue = 95),
  "indianred1","204","#ff5f87",c(red = 255, green = 95, blue = 135),
  "hotpink1","205","#ff5faf",c(red = 255, green = 95, blue = 175),
  "hotpink","206","#ff5fd7",c(red = 255, green = 95, blue = 215),
  "mediumorchid1","207","#ff5fff",c(red = 255, green = 95, blue = 255),
  "darkorange","208","#ff8700",c(red = 255, green = 135, blue = 0),
  "salmon1","209","#ff875f",c(red = 255, green = 135, blue = 95),
  "lightcoral","210","#ff8787",c(red = 255, green = 135, blue = 135),
  "palevioletred1","211","#ff87af",c(red = 255, green = 135, blue = 175),
  "orchid2","212","#ff87d7",c(red = 255, green = 135, blue = 215),
  "orchid1","213","#ff87ff",c(red = 255, green = 135, blue = 255),
  "orange1","214","#ffaf00",c(red = 255, green = 175, blue = 0),
  "sandybrown","215","#ffaf5f",c(red = 255, green = 175, blue = 95),
  "lightsalmon1","216","#ffaf87",c(red = 255, green = 175, blue = 135),
  "lightpink1","217","#ffafaf",c(red = 255, green = 175, blue = 175),
  "pink1","218","#ffafd7",c(red = 255, green = 175, blue = 215),
  "plum1","219","#ffafff",c(red = 255, green = 175, blue = 255),
  "gold1","220","#ffd700",c(red = 255, green = 215, blue = 0),
  "lightgoldenrod3","221","#ffd75f",c(red = 255, green = 215, blue = 95),
  "lightgoldenrod2","222","#ffd787",c(red = 255, green = 215, blue = 135),
  "navajowhite1","223","#ffd7af",c(red = 255, green = 215, blue = 175),
  "mistyrose1","224","#ffd7d7",c(red = 255, green = 215, blue = 215),
  "thistle1","225","#ffd7ff",c(red = 255, green = 215, blue = 255),
  "yellow1","226","#ffff00",c(red = 255, green = 255, blue = 0),
  "lightgoldenrod1","227","#ffff5f",c(red = 255, green = 255, blue = 95),
  "khaki1","228","#ffff87",c(red = 255, green = 255, blue = 135),
  "wheat1","229","#ffffaf",c(red = 255, green = 255, blue = 175),
  "cornsilk1","230","#ffffd7",c(red = 255, green = 255, blue = 215),
  "grey100","231","#ffffff",c(red = 255, green = 255, blue = 255),
  "grey3","232","#080808",c(red = 8, green = 8, blue = 8),
  "grey7","233","#121212",c(red = 18, green = 18, blue = 18),
  "grey11","234","#1c1c1c",c(red = 28, green = 28, blue = 28),
  "grey15","235","#262626",c(red = 38, green = 38, blue = 38),
  "grey19","236","#303030",c(red = 48, green = 48, blue = 48),
  "grey23","237","#3a3a3a",c(red = 58, green = 58, blue = 58),
  "grey27","238","#444444",c(red = 68, green = 68, blue = 68),
  "grey30","239","#4e4e4e",c(red = 78, green = 78, blue = 78),
  "grey35","240","#585858",c(red = 88, green = 88, blue = 88),
  "grey39","241","#626262",c(red = 98, green = 98, blue = 98),
  "grey42","242","#6c6c6c",c(red = 108, green = 108, blue = 108),
  "grey46","243","#767676",c(red = 118, green = 118, blue = 118),
  "grey50","244","#808080",c(red = 128, green = 128, blue = 128),
  "grey54","245","#8a8a8a",c(red = 138, green = 138, blue = 138),
  "grey58","246","#949494",c(red = 148, green = 148, blue = 148),
  "grey62","247","#9e9e9e",c(red = 158, green = 158, blue = 158),
  "grey66","248","#a8a8a8",c(red = 168, green = 168, blue = 168),
  "grey70","249","#b2b2b2",c(red = 178, green = 178, blue = 178),
  "grey74","250","#bcbcbc",c(red = 188, green = 188, blue = 188),
  "grey78","251","#c6c6c6",c(red = 198, green = 198, blue = 198),
  "grey82","252","#d0d0d0",c(red = 208, green = 208, blue = 208),
  "grey85","253","#dadada",c(red = 218, green = 218, blue = 218),
  "grey89","254","#e4e4e4",c(red = 228, green = 228, blue = 228),
  "grey93","255","#eeeeee",c(red = 238, green = 238, blue = 238))
