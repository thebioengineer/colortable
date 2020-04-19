#' knit_print method for color_vctrs
#' @export
#' @keywords internal
#' @importFrom knitr knit_print asis_output
knit_print.color_vctr <- function(x, inline = FALSE, ...){
  if (inline) {
    format(x, ...)
  }else{
    asis_output(knit_vctr_output(x, ...))
  }
}

#' knit_print method for data.frames
#' @export
#' @keywords internal
#' @importFrom knitr kable knit_print asis_output
knit_print.data.frame <- function(x, options, ...){
  df_color_vctr <- as.data.frame(lapply(as.list(x),format,...))
  rownames(df_color_vctr) <- rownames(x)
  formatted_table <- paste(kable(df_color_vctr),collapse = "\n")
  asis_output(formatted_table)
}


#' @importFrom knitr asis_output knit_print
knit_vctr_output <- function(x, ..., method = print_method(), print_width = options()$width){
  formatted_x <- format(x, method = method)
  formatted_x <- format_console_vctr_print(x, formatted_x, console_width = print_width)
  pre_wrap(formatted_x, method = method)
}

pre_wrap <- function(x, ..., method = print_method()){
  pre_wrap_method <- switch(
    method,
    "latex" = pre_wrap.latex,
    "beamer" = pre_wrap.latex,
    "html" = pre_wrap.html,
    "gfm" = pre_wrap.html,
    c
  )
  pre_wrap_method(x, ...)
}

pre_wrap.latex <- function(x,...){
  c("\\begin{Verbatim}[commandchars=\\\\\\{\\}]\n",
    paste0("## ",x,"\n"),
    "\\end{Verbatim}\n")
}

pre_wrap.html <- function(x,...){
  c("<pre>","<code class = \"hljs\">",
    paste("<span>##",x,"</span><br>"),
    "</code>","</pre>")
}
