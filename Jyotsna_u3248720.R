#install.packages("tidyverse")
#install.packages("rvest")
install.packages("stringr")

# load library
library(tidyverse)
library(rvest)
library(stringr)

# ******************************  PART1 ****************************************


# -----------------------------QUESTION 1---------------------------------------
#Q1 Retrieve and load all the data from the url into R

# Specify the URL to scrape
url <- "https://en.wikipedia.org/wiki/List_of_World_Heritage_in_Danger"

html <- read_html(url)

# -----------------------------QUESTION 2---------------------------------------
#Q2 Obtain the table legend and store all its elements.

table_legend <- html %>% html_nodes ('dl') %>% 
  html_text () 

print(table_legend) 

required <- table_legend[2]



# split the single string into multiple strings using the new lines (\n)
my_table <- strsplit(required,"\\n")
my_table


# convert string to a table
my_table <- matrix(unlist(my_table), ncol=1, byrow = TRUE)
View(my_table)


# removing superscript text in square brackets

nchar(my_table[1,])
my_table[1,] <- substring(my_table[1,],1,47)

nchar(my_table[2,])
my_table[2,] <- substring(my_table[2,],1,95)

# change row names using stringr::word

rownames(my_table) <- word(my_table[,1], 1)

#remove first word of observations using regular expression
# ^ is an anchor that matches the beginning of the string.
#\\w+ matches one or more word characters (letters, digits, or underscores).
#: matches the colon character.
#\\s matches empty space in the beginning

my_table[,1] <- sub("^\\w+:\\s", "", my_table[,1])
my_table
#CHK THIS-------------------------------------------------------------------------------
input_string <- "Name: as listed by the World Heritage Committee"

# remove the first word
output_string <- str_replace(input_string, "^\\w+:\\s", "")

# print the updated string
print(output_string)






# -----------------------------QUESTION 3---------------------------------------
#Q3 Scrape the endangered list, which contains the current listed sites.



tables <- html %>% html_table(fill = TRUE)


table <- tables[[2]]


print(table)

# -----------------------------QUESTION 4---------------------------------------
#Scrape all available hyperlinks in the url.

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

# -----------------------------QUESTION 7---------------------------------------



# --------------------------------------------------------------------