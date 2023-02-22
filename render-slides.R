# Revise date and lec_no -------------------------------------------------------
date <- "0223"
lec_no <- "10"


# Run the following code -------------------------------------------------------
f_origin <- "/Users/byeong-hakchoe/Documents/website/xaringan_slides/lec/DANL"
f_dest <- "/Users/byeong-hakchoe/Documents/website/bcdanl.github.io/lec/DANL"

year <- "_2023"
course <- c(200, 210, 310)
lec <- "_Lec"
rmd <- ".Rmd"
html <- ".html"
files <- "_files/figure-html"

# 
# rmarkdown::render(
#   paste0(f_origin, course[2], lec, lec_no, year, date, rmd),
#   output_file = paste0(f_origin, course[2], lec, lec_no, year, date, html),
#   output_dir = "/Users/byeong-hakchoe/Documents/website/bcdanl.github.io/lec"
# )



for (i in 1:3){
  rmarkdown::render( paste0(f_origin, course[i], lec, lec_no, year, date, rmd) )
  file.copy( from = paste0(f_origin, course[i], lec, lec_no, year, date, html),
             to = paste0(f_dest, course[i], lec, lec_no, year, date, html) )

  fig_files <- list.files( 
                    path = paste0(f_origin, course[i], lec, lec_no, year, date, files),
                    all.files = TRUE,
                    full.names = TRUE ) 
       newlocation <- paste0( f_dest, course[i], lec, lec_no, year, date, files )
       dir.create(newlocation, recursive = TRUE)
       file.copy( from = fig_files, 
                  to = newlocation, 
                  overwrite = TRUE, recursive = FALSE, 
                  copy.mode = TRUE)
}






