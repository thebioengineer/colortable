#' Merge semantics for colortable vector arithmetic
#'
#' Set how tables with different styling should merge,
#' by deferring to left styling when it exists, right styling,
#' or attempt to blend them together.
#'
#' @param precedence one of three options: left, right or blended
#'
#' @export
set_color_vctr_precedence <- function(precedence = c("left","right","blended","mixed")){
  precedence <- match.arg(precedence)
  options("colortable.precidence" = precedence)
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
    ((rgb_vect[, 1] - to_match[[1]])) ^ 2 + # red
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


unify_colors <- function(x, type = print_method()) {
  if (grepl("^#", x) &
      grepl("^#[0-9A-Fa-f]{6}$", x, perl = TRUE)) {
    return(toupper(x))
  } else {

    if(!is.null(type)){
      type <- match.arg(type, c("latex", "html", "console"))
      method_colors <- valid_colors(type)$`Color Name`
      in_method_colors <-x %in% method_colors
    }else{
      in_method_colors = FALSE
    }


    if (!in_method_colors) {
      other_colors <- valid_colors(setdiff(c("latex", "html", "console"), type))
      idx <- which(other_colors$`Color Name` %in% x)
      if (length(idx) > 0) {
        return(other_colors$`Hex Code`[min(idx)])
      } else {
        stop(
          paste(
            "Invalid Color Name being used. check for valid color names using `valid_colors( type =",
            type,
            " )`"
          )
        )
      }
    } else{
      return(x)
    }
  }
}
