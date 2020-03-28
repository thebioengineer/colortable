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
    color_scaler(x)
  }
}


scale_col_type <- function(x) {
  ifelse(
    inherits(x,c("numeric","integer","Date", "POSIXt")),
    "continuous",
    "binned"
    )
}

