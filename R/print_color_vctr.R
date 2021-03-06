#' @title  print method for color_vctr
#' @param x object of colortable vector
#' @param ... print options to be passed on
#' @param method The output type to print to. Defaults to one of: "console","latex","html".
#' @export
#' @importFrom cli cat_line
print.color_vctr <- function(x, ..., method = print_method()){
  formatted_x <- format(x, ..., method = method)
  if(method == "console"){
    formatted_x <- format_console_vctr_print(x, formatted_x, ...)
  }
  cli::cat_line(formatted_x)
  invisible(x)
}

#' @title  format method for color_vctror
#' @param x object of colortable cell
#' @param ... format options to be passed on
#' @param method The output type to print to. Defaults to one of: "console","latex","html".
#'
#' @export
format.color_vctr <- function(x, ..., method = print_method()){
  format_method <- switch(method,
         "console" = format.color_vctr.console,
         "latex" = format.color_vctr.latex,
         "beamer" = format.color_vctr.latex,
         "html" = format.color_vctr.html,
         "gfm" = format.color_vctr.html,
         "docx" = format.color_vctr.docx,
         "slidy" = format.color_vctr.html,
         stop("Method for ", print_method()," not implemented yet.")
  )

  format_method(x, ..., method = method)
}

format.color_vctr.console <- function(x,..., method = print_method()){
  x <-
    style2consoleV(
      format_preserve_na(field(x, "vctr"), ...),
      field(x, ".style"),
      field(x, ".text_color"),
      field(x, ".background")
    )
  names(x) <- NULL
  x
}

format.color_vctr.html <- function(x,..., method = print_method()){
  x <-
    style2htmlV(
      format_preserve_na(field(x, "vctr"), ...),
      field(x, ".style"),
      field(x, ".text_color"),
      field(x, ".background")
    )
  names(x) <- NULL
  x
}

format.color_vctr.latex <- function(x,..., escape = TRUE, method = print_method()){
  x <-
    style2texV(
      format_preserve_na(field(x, "vctr"), ..., escape = escape),
      field(x, ".style"),
      field(x, ".text_color"),
      field(x, ".background"),
      method = method
    )
  names(x) <- NULL
  x
}

format.color_vctr.docx <- function(x,..., wrap = TRUE, method = print_method()){
  x <-
    style2docxV(
      format_preserve_na(field(x, "vctr"), ...),
      field(x, ".style"),
      field(x, ".text_color"),
      field(x, ".background")
    )
  names(x) <- NULL
  if(wrap){
      x <- style_zipper_docx(x)
  }
  x
}

format_preserve_na <- function(x, ..., escape = FALSE) {
  f_x <- format(x, ...)
  if (anyNA(x)) {
    f_x[is.na(x)] <- NA
  }
  if(escape){
    f_x <- escape_chars(f_x)
  }
  f_x
}

#' format color_vctr vector printing to console
#' @rdname format.color_vctr
#' @param x color_vctr to be printed
#' @param formatted_x formatted color_vctr for printing
#' @param ... additional parameters passed to `format`
#' @param console_width define nchar wide to print. Default to detecting width
#' @param space the defined spacer between elements. defaults to " ".
format_console_vctr_print <- function(x,formatted_x,...,console_width = options()$width, space = " "){

  x2 <- field(x,"vctr")

  if (length(x) > 0) {
    length_x2 <- length(x2)
    length_x2 <- ifelse(length_x2 > 1000, 1000, length_x2)

    if (any(c("factor","Date","POSIXlt") %in% class(x2))) {
      format_info <- format.info(as.character(x2), ...)
    } else{
      format_info <- format.info(x2, ...)
    }
    n_per_row <-
      max(floor((console_width - 5) / (format_info[1] + ifelse(is.factor(x2),0, 1))),1)
    n_row <- ceiling(length_x2 / n_per_row)

    output_vect <- vector("character", length = n_row)
    idx <- seq(1, length(x2), by = n_per_row)
    prefix <- formatC(paste0("[", idx, "]"), width = max(nchar(idx)) + 2)
    for (i in seq_along(idx)) {
      idx_start <- idx[i]
      idx_end <- idx_start + n_per_row - 1
      if (idx_end > length(x2)) {
        idx_end <- length(x2)
      }
      output_vect[i] <-
        paste(c(prefix[i], formatted_x[idx_start:idx_end]), collapse = space)
    }
  }else{

    output_vect <- paste0("color_vctr<",class(x2),">(0)")
  }
  if (inherits(x2,"factor")) {

    maxl <-  TRUE

    lev <- encodeString(levels(x2), quote = "")
    n <- length(lev)
    colsep <- " "
    T0 <- "Levels:"
    maxl <- {
      width <- console_width - (nchar(T0, "w") + 3L + 1L + 3L)
      lenl <- cumsum(nchar(lev, "w") + nchar(colsep, "w"))
      if (n <= 1L || lenl[n] <= width)
        n
      else max(1L, which.max(lenl > width) - 1L)
    }

    drop <- n > maxl

    factor_init <- if(drop){
      paste(paste(format(n), ""), T0)
    }else{
      T0
    }

    factor_levels <- if(drop){
      c(lev[1L:max(1, maxl - 1)],"...", ifelse(maxl > 1,lev[n],""))
    }else{
      lev
    }

    output_vect <- c(output_vect,
                     paste0(c(factor_init, factor_levels), collapse = colsep))

  }

  output_vect
}

#' Protect control characters in a string for use in a latex table or caption
#'
#' escape takes a vector of chracter values, and puts "\\" in front of them to make them
#' allowable in the latex table output
#'
#' @param x character vector of test containing values that need to be latex escaped
#'
#' @return character Vector of transformed values for table output
#
#' @examples
#' value_example <- c("testvalue", "test_value", "ampersand&")
#' escape_chars(value_example)
#' escape_chars("String_Entry %")
#'
#' @export
escape_chars<-function(x){
  gsub("([&%$#_{}~^\\])","\\\\\\1",x)
}

