
#install.packages("rvest")

# load library
library(tidyverse)
library(rvest)


# ************************  PART1 ************************


# -----------------------------QUESTION 1---------------------------------------
# Specify the URL to scrape
url <- "https://en.wikipedia.org/wiki/List_of_World_Heritage_in_Danger"

html <- read_html(url)

# -----------------------------QUESTION 2---------------------------------------


table_legend <- html %>% html_nodes ('dl') %>% 
  html_text () 

print(table_legend) 

# -----------------------------QUESTION 3---------------------------------------
tables <- html %>% html_table(fill = TRUE)


table <- tables[[2]]


print(table)

# -----------------------------QUESTION 4---------------------------------------