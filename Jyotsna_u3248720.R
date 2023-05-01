
# Assignment 2: Web Scraping, Data Cleaning and Analysis in R
# subject name: 11520G Data Capture and Preparations
# created by: Jyotsna, ID: u3248720

# ---------------------------Data description:----------------------------------

# For this assignment we need to collect specific data from the United Nations
# Educational, Scientific and Cultural Organisation (UNESCO). This organisation
# supports the preservation of the world’s natural and cultural heritage.
# As of today (April 2022), there are more than 1100 heritage sites around the
# world, most of which are man-made like the Acropolis of Athens or the Taj
# Mahal in India, but also natural sites like the Great Barrier Reef here in
# Australia are listed. Unfortunately, some of the sites in the list are
# threatened by human intervention or natural reasons. We want to know more
# about the endangered sites, their location, and the reasons that put a site
# at risk.


# ------------------------------------------------------------------------------
#Load following packages to proceed

#install.packages("tidyverse")
#install.packages("rvest")
#install.packages("stringr")

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
my_table[1,] <- substring(my_table[1,],1,47) #nchar-4

nchar(my_table[2,])
my_table[2,] <- substring(my_table[2,],1,95) #nchar-6

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

all_url <- html %>%
  html_nodes("a") %>%
  html_attr("href")


# Extract the link text
link_text <- html %>%
  html_nodes("a") %>%
  html_text()


scrape_all <- data.frame(CONTENT = link_text, URL = all_url)


# -----------------------------QUESTION 5---------------------------------------







# -----------------------------QUESTION 6---------------------------------------

# -----------------------------QUESTION 7---------------------------------------



# --------------------------------------------------------------------