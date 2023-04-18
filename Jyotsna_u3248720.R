# Load required packages
library(rvest)
library(dplyr)

# Specify the URL to scrape
url <- "https://en.wikipedia.org/wiki/List_of_World_Heritage_in_Danger"

# Read the HTML content from the URL
html <- read_html(url)

# Extract the table from the HTML content
table <- html %>%
  html_nodes("table") %>%  # select all tables on the page
  .[[1]] %>%  # select the first table
  html_table(fill = TRUE)  # convert the table to a data frame

# Print the resulting data frame
print(table)