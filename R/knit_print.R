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
  knit_vctr_output_fun <- switch (method,
    docx = knit_vctr_output.docx,
    knit_vctr_output.default
  )
  knit_vctr_output_fun(x, ..., method = method, print_width = print_width)
}

knit_vctr_output.default <- function(x, ..., method = print_method(), print_width = options()$width){
  formatted_x <- format(x,..., method = method)
  formatted_x <- format_console_vctr_print(x, formatted_x, console_width = print_width)
  pre_wrap(formatted_x, method = method)
}

knit_vctr_output.docx <- function(x, ..., method = print_method(), print_width = options()$width){
  formatted_x <- format(x,..., method = method, wrap = FALSE)
  formatted_x <-
    format_console_vctr_print(x, formatted_x, console_width = print_width, space = "<w:r><w:t xml:space=\"preserve\"> </w:t></w:r>")
  pre_wrap(formatted_x, method = method)
}


pre_wrap <- function(x, ..., method = print_method()){
  pre_wrap_method <- switch(
    method,
    "latex" = pre_wrap.latex,
    "beamer" = pre_wrap.latex,
    "html" = pre_wrap.html,
    "gfm" = pre_wrap.html,
    "docx" = pre_wrap.docx,
<<<<<<< HEAD
=======
    "slidy" = pre_wrap.html,
>>>>>>> master
    c
  )
  pre_wrap_method(x, ...)
}

<<<<<<< HEAD
pre_wrap.latex <- function(x,...){
  c("\\begin{Verbatim}[commandchars=\\\\\\{\\}]\n",
    paste0("## ",x,"\n"),
    "\\end{Verbatim}\n")
}

pre_wrap.html <- function(x,...){
  c("<pre>","<code class = \"hljs\">",
    paste("<span>##",x,"</span><br>"),
=======
pre_wrap.latex <- function(x, ...) {
  gsub(" ","\\ ",(paste0("\\texttt{\\#\\# ", x,"}\\newline\n")),fixed = TRUE)
}


pre_wrap.html <- function(x,...){
  c("<pre>","<code class = \"hljs\">",
    paste("<div class='remark-code-line'><span>##",x,"</span></div>"),
>>>>>>> master
    "</code>","</pre>")
}

pre_wrap.docx <- function(x, ...) {

  x2 <- do.call('c',lapply(regmatches(x, regexpr("]" , x,), invert = TRUE),
         function(x){
           paste0(
             "<w:p>",
             "<w:pPr><w:pStyle w:val=\"SourceCode\" /></w:pPr>",
             "<w:r>",
             "<w:rPr><w:rStyle w:val=\"NormalTok\" /></w:rPr>",
             "<w:t xml:space=\"preserve\">## ",paste0(x[1],"]"),"</w:t></w:r>",
             x[2],
             "</w:p>")
         }))


  paste0("```{=openxml}\n",
         x2,
        "\n```",
        collapse="\n")
}
