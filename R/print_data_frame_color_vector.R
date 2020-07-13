#' wrapper to print data.frame colors
#'
#' custom print method for data.frames that contain color_vctr's. Uses the
#' base print.data.frame otherwise
#'
#' @param x a data.frame
#' @param ... optional arguments to print or plot methods.
#' @param digits the minimum number of significant digits to be used: see print.default.
#' @param quote logical, indicating whether or not entries should be printed with surrounding quotes.
#' @param right logical, indicating whether or not strings should be right-aligned. The default is right-alignment.
#' @param row.names logical (or character vector), indicating whether (or what) row names should be printed.
#' @param max numeric or NULL, specifying the maximal number of entries to be printed. By default, when NULL, getOption("max.print") used.
#'
print.data.frame_shim <- function(x, ..., digits = NULL, quote = FALSE, right = TRUE,
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

    omit <- (n0 <- max%/%nrow(x)) < n

    cat_line(format_colortable(x, max = max, digits = digits,...))

  }
  invisible(x)
}


format_colortable <- function(x, max = getOption("max.print", 99999L), digits = NULL, ..., width = options()$width){

  n <- nrow(x)
  omit <- (n0 <- n %/% max) > 0

  m <- format.data.frame(if (omit)
    x[seq_len(max), , drop = FALSE]
    else x, digits = digits, na.encode = FALSE, ...)

  calc_col_widths <-
    lapply(1:ncol(x), function(idx, df){
      c(
        col = get_format_info(df[[idx]]),
        name = format.info(colnames(df)[[idx]])[1]
      )
    },if (omit)
      x[seq_len(max), , drop = FALSE]
    else x)

  col_widths <- sapply(calc_col_widths, function(widths){ max(widths) +1})
  content_col_widths <- sapply(calc_col_widths, `[`, 1)

  rownames_width <- format.info(rownames(x))

  col_type_num <- sapply(x, function(z){
    if(is_color_vctr(z)){
      z <- field(z, "vctr")
    }
    is.numeric(z)
  })


  n_df_chunks <- rep(1, ncol(x))
  chunk_idx <- 1
  chunk_width <- rownames_width

  for( column_i in 1:ncol(x) ){
    this_col_width <- col_widths[column_i]

    if( (chunk_width + this_col_width) < width){
      n_df_chunks[column_i] <- chunk_idx
      chunk_width <- chunk_width + this_col_width

    } else if ( this_col_width > (width - rownames_width) ){
      n_df_chunks[column_i] <- chunk_idx + 1
      chunk_idx <- chunk_idx + 2
      chunk_width <- rownames_width
    } else {
      chunk_idx <- chunk_idx + 1
      n_df_chunks[column_i] <- chunk_idx
      chunk_width <- rownames_width + this_col_width
    }
  }

  n_df_chunks <- split(1:ncol(x),n_df_chunks)

  body <-
    do.call('c', lapply(n_df_chunks, function(df_chunk_cols) {
      header <-
        paste0(c(pad("", rownames_width),
                 pad(colnames(x)[df_chunk_cols], col_widths[df_chunk_cols])),
               collapse = "")

      body <-
        sapply(1:nrow(m), function(row,
                                   m,
                                   x,
                                   row_names,
                                   col_widths,
                                   content_col_widths,
                                   col_type_num) {
          row_out <- pad(row_names[row], rownames_width)

          content_pad <-
            (col_widths - content_col_widths) +
            ((content_col_widths - sapply(lapply(
              x[row, , drop = TRUE], get_format_info
            ), `[`, 1)) * !col_type_num)

          content <-
            to_pad(format(m[row, , drop = TRUE]), content_pad)

          paste0(c(row_out, content), collapse = "")

        },
        m[, df_chunk_cols, drop = FALSE],
        x[, df_chunk_cols, drop = FALSE],
        rownames(x),
        col_widths[df_chunk_cols],
        content_col_widths[df_chunk_cols],
        col_type_num[df_chunk_cols])

      c(header, body)
    }))

  if(omit){
    body <- c(
      body,
      paste0(
        c(" [ reached 'max' / getOption(\"max.print\") -- omitted ",
          n - n0, " rows ]"), collapse="")
    )
  }

  body
}

pad <- function(x, padding = 0) {
  sapply(seq_along(x), function(idx, x, padding) {
    formatC(x[idx], width = if (length(padding) == 1)
      padding
      else
        padding[idx])
  }, x, padding)
}

to_pad <- function(x, padding = 0) {
  sapply(seq_along(x), function(idx, x, padding) {

    paste0(c(rep(" ", if (length(padding) == 1)

      padding
      else
        padding[idx]),
      x[idx]),
      collapse = "")
  }, x, padding)
}

get_format_info <- function(x){
  if(is_color_vctr(x)){
    x <- field(x,"vctr")
  }
  if (is.factor(x) & !any(is.na(x))){
    format.info(as.character(x))
  }else if (all(is.na(x))) {
    2
  }else {
    format.info(x)
  }
}
