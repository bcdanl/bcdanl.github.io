# install.packages("renderthis")
# remotes::install_github("jhelvy/xaringanBuilder", force = TRUE)
# remotes::install_github('rstudio/chromote')
library(renderthis)
slide_list <- list.files('/Users/byeong-hakchoe/Documents/website/bcdanl.github.io/lec',
                         pattern = 'html')
slide_list <- filter(as_tibble(slide_list),
                     str_detect(value, "DANL100") | str_detect(value, "DANL200") )
slide_list <- as_vector(slide_list)
for ( i in 1:length(slide_list) ) {
  to_pdf(from = paste("https://bcdanl.github.io//lec/", slide_list[i], 
                      sep = ""))
}

