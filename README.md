
<!-- README.md is generated from README.Rmd. Please edit that file -->

# colortable

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
<!-- badges: end -->

The goal of colortable is to make it easier to color and style your
tables.

Current styling technologies such as kableExtra and formattable make you
go to your final printing status before you can see the coloring and
styling applied. This adds overhead and iterating on outputs and
sometimes require compiling entire reports to check minute changes.
Alternatively, one can hard-code the styling into the table, but this
then requires complete changes to data types in the tables.

{{colortable}} solves this conundrum by allowing the user to keep the
types of the contents, and applies the style updates on printing.

## Installation

<!-- You can install the released version of colortable from [CRAN](https://CRAN.R-project.org) with: -->

Currently {{colortable}} is only available on github, and is very much
under development.

``` r
remotes::install_github("thebioengineer/colortable")
## install.packages("colortable") ## Not Available on CRAN
```

## Example

A common case I have seen for coloring values is from analysis coloring
p-values. Normally, when I have seen this the color is hard-coded in an
ifelse statement with a paste0.

Not only is hardcoding a bad practice, but then the outputs are not
flexible.

``` r
suppressPackageStartupMessages({
  library(tidyverse)
  library(colortable)
  library(knitr)})

## Super Great analysis of mtcars!

lm_fit <- lm(mpg ~ ., mtcars)

a_lm_fit <- anova(lm_fit)

tbl_anova <- as_tibble(a_lm_fit)%>% 
  mutate(
    Coef     = rownames(a_lm_fit),
    `Pr(>F)` = set_styling(`Pr(>F)`, `Pr(>F)` < 0.05, background = "green", style = "underline"),
    `Pr(>F)` = set_styling(`Pr(>F)`, is.na(`Pr(>F)`), style = "strikethrough", text_color = "silver"),
    `F value` = set_styling(`F value`, is.na(`F value`), style = "strikethrough", text_color = "silver")
  ) %>% 
  select(Coef, everything())

kable(tbl_anova, escape = FALSE)
```

| Coef      | Df |      Sum Sq |     Mean Sq |                                                            F value |                                                                      Pr(\>F) |
| :-------- | -: | ----------: | ----------: | -----------------------------------------------------------------: | ---------------------------------------------------------------------------: |
| cyl       |  1 | 817.7129524 | 817.7129524 |                                     <span style="">116.4245</span> | <span style="text-decoration:underline;background:green;">5.03445e-10</span> |
| disp      |  1 |  37.5939529 |  37.5939529 |                                     <span style="">5.352562</span> |  <span style="text-decoration:underline;background:green;">0.03091083</span> |
| hp        |  1 |   9.3709293 |   9.3709293 |                                     <span style="">1.334217</span> |                                               <span style="">0.261031</span> |
| drat      |  1 |  16.4674349 |  16.4674349 |                                     <span style="">2.344605</span> |                                              <span style="">0.1406438</span> |
| wt        |  1 |  77.4757948 |  77.4757948 |                                     <span style="">11.03087</span> | <span style="text-decoration:underline;background:green;">0.003244492</span> |
| qsec      |  1 |   3.9493082 |   3.9493082 |                                    <span style="">0.5622956</span> |                                              <span style="">0.4616557</span> |
| vs        |  1 |   0.1297687 |   0.1297687 |                                   <span style="">0.01847624</span> |                                              <span style="">0.8931733</span> |
| am        |  1 |  14.4742372 |  14.4742372 |                                     <span style="">2.060817</span> |                                              <span style="">0.1658577</span> |
| gear      |  1 |   0.9717105 |   0.9717105 |                                    <span style="">0.1383504</span> |                                              <span style="">0.7136533</span> |
| carb      |  1 |   0.4066688 |   0.4066688 |                                   <span style="">0.05790079</span> |                                              <span style="">0.8121787</span> |
| Residuals | 21 | 147.4944300 |   7.0235443 | <span style="text-decoration:line-through;color:silver;">NA</span> |           <span style="text-decoration:line-through;color:silver;">NA</span> |

There are many more use-cases and code surrounding this package, but I
feel like this would be the most common use-case. Let me know if you
come up with more\!

## The Nuts and Bolts

{{colortable}} works by making a special S3 class called a
`colortable_vect`, and custom print/format functions. Currently it only
works with tibbles, but there are plans to make it friendly with
data.frames.

## Inspiration

This idea was inspired by [`crayon`](https://github.com/r-lib/crayon),
and has some elements based on it. I thank all the developers of that
project\!

## COC

Please note that the ‘colortable’ project is released with a
[Contributor Code of Conduct](CODE_OF_CONDUCT.md). By contributing to
this project, you agree to abide by its terms.
