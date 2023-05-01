
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
# Load following packages to proceed

# install.packages("tidyverse")
# install.packages("rvest")
# install.packages("stringr")

# load library
library(tidyverse)
library(rvest)
library(stringr)

# ******************************  PART1 ****************************************


# -----------------------------QUESTION 1---------------------------------------

# Q1 Retrieve and load all the data from the url into R

# Specify the URL to scrape
url <- "https://en.wikipedia.org/wiki/List_of_World_Heritage_in_Danger"

webpage <- read_html(url)

# -----------------------------QUESTION 2---------------------------------------

# Q2 Obtain the table legend and store all its elements.

table_legend <- webpage %>%
  html_nodes("dl") %>%
  html_text()

print(table_legend)

required <- table_legend[2]



# split the single string into multiple strings using the new lines (\n)
legend_table <- strsplit(required, "\\n")
legend_table


# convert string to a table
legend_table <- matrix(unlist(legend_table), ncol = 1, byrow = TRUE)
View(legend_table)


# removing superscript text in square brackets

nchar(legend_table[1, ])
legend_table[1, ] <- substring(legend_table[1, ], 1, 47) # nchar-4

nchar(legend_table[2, ])
legend_table[2, ] <- substring(legend_table[2, ], 1, 95) # nchar-6

# change row names using stringr::word

rownames(legend_table) <- word(legend_table[, 1], 1)

# remove first word of observations using regular expression
# ^ is an anchor that matches the beginning of the string.
# \\w+ matches one or more word characters (letters, digits, or underscores).
# : matches the colon character.
# \\s matches empty space in the beginning

legend_table[, 1] <- sub("^\\w+:\\s", "", legend_table[, 1])
legend_table
# CHK THIS-------------------------------------------------------------------------------
input_string <- "Name: as listed by the World Heritage Committee"

# remove the first word
output_string <- str_replace(input_string, "^\\w+:\\s", "")

# print the updated string
print(output_string)






# -----------------------------QUESTION 3---------------------------------------

# Q3 Scrape the endangered list, which contains the current listed sites.



tables <- webpage %>% html_table(fill = TRUE)


endangered_table <- tables[[2]]


print(endangered_table)

# -----------------------------QUESTION 4---------------------------------------

# Q4 Scrape all available hyperlinks in the url.

all_url <- webpage %>%
  html_nodes("a") %>%
  html_attr("href")


# Extract the link text
link_text <- webpage %>%
  html_nodes("a") %>%
  html_text()

# CHK THIS-------------------------------------------------------------------------------
scrape_all <- data.frame(CONTENT = link_text, URL = all_url)


# -----------------------------QUESTION 5---------------------------------------

# Q5 Using computational methods, obtain the hyperlink that you can click on to
# jump to a new page that contains the selection criteria

req_index <- which(grepl("Criteria", scrape_all$CONTENT))
req_list <- scrape_all[req_index, ]
# CHK THIS-------------------------------------------------------------------------------
hyperlink_criteria <- req_list[1, 2]
hyperlink_criteria <- paste0("https://en.wikipedia.org", hyperlink_criteria)

# problem is that same text gets pasted repeatedly everytime me run the above line


# CHK THIS-------------------------------------------------------------------------------
req_index <- list()
req_link <- list()

for (i in 1:nrow(scrape_all)) {
  req_index[[i]] <- which(grepl("Criteria", scrape_all$CONTENT))
  # req_link[i] <- scrape_all$URL[req_index]
}



# -----------------------------QUESTION 6---------------------------------------

# Q6 Use the hyperlink obtained in the previous step and scrape the two lists
# (cultural and natural) and store them in two data structures within a list

criteria_page <- read_html(hyperlink_criteria)


all_text <- list()
selection_criteria <- list()
for (i in 1:2) {
  all_text <- criteria_page %>%
    html_nodes("ol") %>%
    html_text()
  selection_criteria[[i]] <- all_text[i]
  # splitting the strings
  selection_criteria[[i]] <- strsplit(selection_criteria[[i]], "\\n")
  # convert to matrix table
  selection_criteria[[i]] <- matrix(unlist(selection_criteria[[i]]), ncol = 1, byrow = TRUE)
}

# checking the tables obtained
view(selection_criteria[[1]])

# DOES THIS NEED FURTHER CLEANING?--------------------------------------------------------------


# ******************************  PART2 ****************************************
# -----------------------------QUESTION 1---------------------------------------

# Q1 From the table containing the endangered sites, remove the undesired variables:
# Image and Refs.





# --------------------------------------------------------------------
