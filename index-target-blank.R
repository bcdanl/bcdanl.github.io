# Replace the values
lec_no <- '01'
lec_date <- '0123'

quarto::quarto_render("index.qmd")

library(stringr)
# Read the HTML file
html_as_text <- readLines("index.html", warn = FALSE)
system(paste("rm", shQuote("index.html")))
# Sys.sleep(2)

# Find the element to replace 
old_content <- str_c('lec-', lec_no, '-2024-', lec_date, '.html"')
new_content <- str_c(old_content, " ", 'target="_blank"') 

html_as_text <- str_replace_all(html_as_text,
                                old_content,
                                new_content)

writeLines(html_as_text, 
           "index.html")



# file.copy("tmp/index.html", "index.html")

# Use system() to execute the rm command
# system(paste("rm", shQuote("index.html")))
# Sys.sleep(2)
# 
# file.rename("index2.html", "index.html")
