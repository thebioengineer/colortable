#' @title print data.frame
#' @description
#'     custom print method for data.frames that contain color_vectors
#'
#' @export
print.data.frame <- function(x, ..., digits = NULL, quote = FALSE, right = TRUE,
                             row.names = TRUE, max = NULL) {
  contains_color_vctr <- any(sapply(as.list(x),is_color_vctr))
  if (contains_color_vctr) {
    print.data.frame.color_vector(x, ..., digits = digits, quote = quote, right = right,
                                  row.names = row.names, max = max)
  } else {
    base::print.data.frame(x, ..., digits = digits, quote = quote, right = right,
                           row.names = row.names, max = max)
  }
}


#' @importFrom cli cat_line
print.data.frame.color_vector <- function (x, ..., digits = NULL, quote = FALSE, right = TRUE,
          row.names = TRUE, max = NULL)
{

  n <- length(row.names(x))
  if (length(x) == 0L) {
    cat(sprintf(ngettext(n, "data frame with 0 columns and %d row",
                         "data frame with 0 columns and %d rows"), n), "\n",
        sep = "")
  } else if (n == 0L) {
    print.default(names(x), quote = FALSE)
    cat(gettext("<0 rows> (or 0-length row.names)\n"))
  } else {
    if (is.null(max))
      max <- getOption("max.print", 99999L)
    if (!is.finite(max))
      stop("invalid 'max' / getOption(\"max.print\"): ",
           max)
    omit <- (n0 <- max%/%length(x)) < n

    m <- as.matrix(format.data.frame(if (omit)
      x[seq_len(n0), , drop = FALSE]
      else x, digits = digits, na.encode = FALSE))

    if (!isTRUE(row.names)){
      dimnames(m)[[1L]] <- if (isFALSE(row.names)) {
        rep.int("", if (omit){ n0 } else { n })
      } else { row.names }
    }

    cat_line(format(m), ..., quote = quote, right = right, max = max)

    if (omit)
      cat(" [ reached 'max' / getOption(\"max.print\") -- omitted",
          n - n0, "rows ]\n")
  }
  invisible(x)
}

