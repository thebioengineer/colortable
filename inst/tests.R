devtools::load_all()
library(tidyverse)

cell_sample  <- color_vctr(24, text_color = "red", background = "blue")
cell_sample2 <- color_vctr(42, background = "yellow")
cell_sample3 <- color_vctr(68, text_color = "magenta", style = "strikethrough")
cell_sample4 <- color_vctr(70, text_color = "green")


print(cell_sample, method = "console")
print(cell_sample, method = "html")
print(cell_sample, method = "latex")

cell_sample
cell_sample2
cell_sample3
cell_sample4

vect_sample <- color_vctr(cell_sample, cell_sample2, cell_sample3, cell_sample4)

vect_sample2 <- vect_sample
vect_sample2[5] <- 422
vect_sample2[20] <- color_vctr(98119, text_color = "yellow", background = "blue", style = "underline")
vect_sample2[6:7] <- c(21,23)
vect_sample2[10:12] <- color_vctr(cell_sample, cell_sample2, cell_sample3)
vect_sample2[13:14] <- color_vctr(cell_sample, cell_sample2, cell_sample3)

random_color_vect <- color_vctr(runif(30), text_color = "blue")
random_text_vect <- color_vctr("test","this","theory here", text_color = "blue")
random_text_vect
as.character(random_text_vect)


print(vect_sample, method = "console")
print(vect_sample, method = "html")
print(vect_sample, method = "latex")

sample_tbl <- tibble(idx = 1:5, z = vect_sample[1:5], z2 =vect_sample2[10:14])
sample_tbl

sample_df <- data.frame(idx = 1:5, vect_sample[1:5], z2 =vect_sample2[10:14])

data.frame(idx = 1:5, vect_sample.1.5. = c(24, 42, 68, 70, NA), z2 = c(24,42,68,24,42))

sample_df

sample_tbl %>%
  mutate(
    col2 = z ,
    col3 = color_vctr(runif(5), background = c("blue","magenta",NA,NA,NA), style = "underline"),
    col4 = runif(5)
    ) %>%
  mutate(
    col5 = set_styling(col4, col4 > .5, text_color = "green", style = "underline")
  )
