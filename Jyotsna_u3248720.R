
#install.packages("rvest")
#install.packages("stringr")

# load library
library(tidyverse)
library(rvest)
library(stringr)

# ******************************  PART1 ****************************************


# -----------------------------QUESTION 1---------------------------------------
# Specify the URL to scrape
url <- "https://en.wikipedia.org/wiki/List_of_World_Heritage_in_Danger"

html <- read_html(url)

# -----------------------------QUESTION 2---------------------------------------


table_legend <- html %>% html_nodes ('dl') %>% 
  html_text () 

print(table_legend) 

required <- table_legend[2]

# remove \n next to parenthesis
required <- gsub("\n","",required)

required

#remove empty spaces
required <- gsub("\\h+", " ", required, perl=TRUE)  #\h is shorthand to match any horizontal whitespaces
required <- str_trim(required)
required

# split the single string into multiple strings using the new lines (\n)
my_table <- strsplit(required,"\\n")
my_table


# convert string to a table
my_table <- matrix(unlist(my_table), ncol=2, byrow = TRUE)
View(my_table)


# -----------------------------QUESTION 3---------------------------------------
tables <- html %>% html_table(fill = TRUE)


table <- tables[[2]]


print(table)

# -----------------------------QUESTION 4---------------------------------------
#Scrape all available hyperlinks in the url.

# installing packages
install.packages("httr")
install.packages("XML")

# importing packages
library(httr)
library(XML)


# making http request
resource <- get(url)

# converting all the data to HTML format
parse < -htmlParse(resource)

# scrapping all the href tags
links < -xpathSApply(parse, path="//a", xmlGetAttr, "href")

# printing links
print(links)
#-----



# -----------------------------QUESTION 5---------------------------------------

# -----------------------------QUESTION 6---------------------------------------