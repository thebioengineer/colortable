#' scale color vector colors based on contents
#'
#' color vector contents based on contents. only to be used within
#' color_vctr with numeric values
#'
#' @param palette pallete to use. A vector of colors to use
#'
#' @importFrom scales col_numeric col_factor
#' @importFrom grDevices palette

scale_color <- function(palette){
  function(x){
    color_scaler <- scales::col_factor(palette,levels = levels(factor(x)))
    colors <- color_scaler(x)
    name_colors(colors)
  }
}

name_colors <- function(hex_colors){
  u_hc <- unique(hex_colors)
  output_colors <- vector("character", length(hex_colors))
  rgb_mat <- col2rgb(u_hc)
  rgb_key <- do.call(rbind,color_key$RGB)
  for(hex_code in 1:length(u_hc)){
    input_rgb <- rgb_mat[,hex_code, drop = TRUE]
    output_colors[hex_colors == u_hc[hex_code]] <-
      color_key$Name[which.min(sqrt((rgb_key[,1] - input_rgb[[1]]) ^ 2 +
                                      (rgb_key[,2] - input_rgb[[2]]) ^ 2 +
                                      (rgb_key[,3] - input_rgb[[3]]) ^ 2
      ))]
  }
  output_colors
}



