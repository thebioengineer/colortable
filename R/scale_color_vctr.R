#' scale color vector colors based on contents
#'
#' @param palette pallete to use. A vector of colors to use
#' @param na.color color to set NA values
#'
#' @importFrom scales col_numeric col_factor
#' @importFrom grDevices palette
#' @export
color_scale <- function(palette, na.color = "#808080") {
  function(x) {
    color_scaler <- switch(
      scale_col_type(x),
      continuous = col_numeric(
        palette,
        domain = c(min(x, na.rm = TRUE),
                   max(x, na.rm = TRUE)),
        na.color = na.color
      ),
      binned = col_factor(
        palette,
        levels = levels(factor(x)),
        na.color = na.color)
    )
    colors <- color_scaler(x)
    name_colors(colors)
  }
}


name_colors <- function(hex_colors) {
  u_hc <- unique(hex_colors)
  output_colors <- vector("character", length(hex_colors))
  rgb_mat <- col2rgb(u_hc)
  rgb_key <- do.call(rbind, color_key$RGB)
  for (hex_code in 1:length(u_hc)) {
    input_rgb <- rgb_mat[, hex_code, drop = TRUE]
    output_colors[hex_colors == u_hc[hex_code]] <-
      color_key$Name[which.min(sqrt((rgb_key[, 1] - input_rgb[[1]]) ^ 2 +
                                      (rgb_key[, 2] - input_rgb[[2]]) ^ 2 +
                                      (rgb_key[, 3] - input_rgb[[3]]) ^ 2
      ))]
  }
  output_colors
}



scale_col_type <- function(x) {
  ifelse(
    inherits(x,c("numeric","integer","Date", "POSIXt")),
    "continuous",
    "binned"
    )
}

