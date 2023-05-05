
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
# library(stringr) #stringr is a part of tidyverse

# ******************************  PART1 ****************************************


# -----------------------------QUESTION 1---------------------------------------

# Q1 Retrieve and load all the data from the url into R

# Specify the URL to scrape
url <- "https://en.wikipedia.org/wiki/List_of_World_Heritage_in_Danger"

#using read_html package from rvest library, scrape the website contents 
webpage <- read_html(url)

# -----------------------------QUESTION 2---------------------------------------

# Q2 Obtain the table legend and store all its elements.

#using the pipe function of tidyverse, and extracting the node with the 
#identifier 'dl' and then keeping all the text that is present within that node.
table_legend <- webpage %>%
  html_nodes("dl") %>%
  html_text()

#checking to see the extracted text. It is in the form of lists.
#print(table_legend)

#taking out the second list since it contains the table legend text required
required <- table_legend[2]


#the legend text is in the form of continuous lines. We need to split the text
# split the single string into multiple strings using the new lines (\n)
#using strsplit function from stringr
legend_table <- strsplit(required, "\\n")
#legend_table #checking the result


#Making the legend presentable by converting it to a table
# convert string to a table using the matrix function. We need only 1 column 
#we need to create the table observations according to the rows
legend_table <- matrix(unlist(legend_table), ncol = 1, byrow = TRUE)
#View(legend_table) #checking the result


# removing superscript text in square brackets which came as part of citations 
# on the given webpage in row 1

# nchar(legend_table[1, ]) gives the number of characters in the string of 1st row
# using substring function we specify start index of desired subsetted string
#we keep the first character so start=1 which is the second argument
#we remove the last 3 characters containing citation text so end=total characters in string
#minus 4 to achieve the desired index
legend_table[1, ] <- substring(legend_table[1, ], 1, (nchar(legend_table[1, ])-4)) 

# removing superscript text in square brackets which came as part of citations 
# on the given webpage in row 2


# nchar(legend_table[2, ]) gives the number of characters in the string of 2nd row
# using substring function we specify start index of desired subsetted string
#we keep the first character so start=1 which is the second argument
#we remove the last 63 characters containing citation text so end=total characters in string
#minus 6 to achieve the desired index
legend_table[2, ] <- substring(legend_table[2, ], 1, (nchar(legend_table[2, ])-6)) 


# change row names using stringr::word to achieve the rownames as the legend entities
rownames(legend_table) <- word(legend_table[, 1], 1)
#editing the rowname with year observation
rownames(legend_table)[5] <- "Year (WHS):"

# remove first word of observations using regular expression
# ^ is an anchor that matches the beginning of the string.
# \\w+ matches one or more word characters (letters, digits, or underscores).
# : matches the colon character.
# \\s matches empty space in the beginning
legend_table[, 1] <- sub("^\\w+:\\s", "", legend_table[, 1])

#removing extra text from 5th row using sub and regex
legend_table[5, 1] <- sub("Year \\(WHS\\):", "", legend_table[5, 1])

#trimming any whitespaces in the column
legend_table[,1] <- str_trim(legend_table[,1], side = "both")

#change column name to 'Description of Table Legend'
colnames(legend_table) <- "Description of Table Legend"

#check final result
view(legend_table)


# -----------------------------QUESTION 3---------------------------------------

# Q3 Scrape the endangered list, which contains the current listed sites.


#using the pipe function and html_table function from rvest package to extract only 
#the tables of the webpage
tables <- webpage %>% html_table(fill = TRUE)

#labelling the table as endangered table and accessing the 2nd table from the list 
#of tables, which is what is desired
endangered_table <- tables[[2]]

#check the achieved table
print(endangered_table)

# -----------------------------QUESTION 4---------------------------------------

# Q4 Scrape all available hyperlinks in the url.

#using the html_nodes function from rvest, we extract the content within the 
#identifier 'a' and then extract the hyperlink text references using 'href' as the identifier
all_url <- webpage %>%
  html_nodes("a") %>%
  html_attr("href")


# Extract the link text again by using the node 'a' as identifier and keeping all 
#text only and not the link by using the html_text function
link_text <- webpage %>%
  html_nodes("a") %>%
  html_text()

#creating a data frame of the achieved content and their links
scrape_all <- data.frame(CONTENT = link_text, URL = all_url)
#resulting in 1416 observations

#removing extra rows
#some rows do contain citation link texts which might work by adding a domain name to them
#hence I am removing only the row 23 with only single character(#)
scrape_all <- scrape_all[-23, ]

# -----------------------------QUESTION 5---------------------------------------

# Q5 Using computational methods, obtain the hyperlink that you can click on to
# jump to a new page that contains the selection criteria

#finding the index number of row with the word "criteria" from the data frame
#containing all the hyperlinks in the webpage
req_index <- which(grepl("Criteria", scrape_all$CONTENT))
#creating a new list with only the required links
req_list <- scrape_all[req_index, ]
#selecting appropriate link
hyperlink_criteria <- req_list[1, 2]

#using paste command to add domain name in order to make link functional
hyperlink_criteria <- paste0("https://en.wikipedia.org", hyperlink_criteria)



# -----------------------------QUESTION 6---------------------------------------

# Q6 Use the hyperlink obtained in the previous step and scrape the two lists
# (cultural and natural) and store them in two data structures within a list

#reading the required hyperlink for criteria page using rvest::read_html
criteria_page <- read_html(hyperlink_criteria)

#creating empty list to hold all text in criteria page under all the ordered lists
all_text <- list()
#creating empty list to hold the required text containing criteria description
selection_criteria <- list()

#initializing for loop to store all the text into lists of list
for (i in 1:2) {
  all_text <- criteria_page %>%
    html_nodes("ol") %>%
    html_text()
  selection_criteria[[i]] <- all_text[i]
  # splitting the strings
  selection_criteria[[i]] <- strsplit(selection_criteria[[i]], "\\n")
  # convert list to matrix table
  selection_criteria[[i]] <- matrix(unlist(selection_criteria[[i]]), ncol = 1, byrow = TRUE)
}

# checking the tables obtained
view(selection_criteria[[1]])
view(selection_criteria[[2]])

# name the columns to identify which criteria belongs to which category
colnames(selection_criteria[[1]]) <- "Cultural"
colnames(selection_criteria[[2]]) <- "Natural"

# ******************************  PART2 ****************************************
# -----------------------------QUESTION 1---------------------------------------

# Q1 From the table containing the endangered sites, remove the undesired variables:
# Image and Refs.

# creating a vector for the column names image and refs in order to get their index numbers using
#which function. After getting their index number we just need to subset the data frame 
#where negative sign in front of column index means that the stated column index is to be removed
endangered_table <- endangered_table[, -which(names(endangered_table) %in% c("Image", "Refs"))]

#check the result
view(endangered_table)

# -----------------------------QUESTION 2---------------------------------------

# Q2 obtain the country from the “Location” variable. Using computational methods
# (e.g., Regex) fix any inconsistencies in the variable and then extract the country only.

# Step-1: remove all characters occuring after a digit using gsub and lookbehind
# (?<=\\d) is a positive lookbehind that matches any character that is preceded by a digit.
# perl=TRUE in gsub to enable the use of lookbehind

endangered_table$Location <- gsub("(?<=\\d).*", "", endangered_table$Location, perl = TRUE)



# Step-2: remove the last character containing the digit which was used in step-1
endangered_table$Location <- str_sub(endangered_table$Location, end = -2)


# Step-3: Fix exceptions in rows 1 and 48
# for location in row 1-remove all text coming after the dot
endangered_table$Location <- gsub("\\..*", "", endangered_table$Location)



# Step-4: remove all characters occuring before the comma using sub and lookahead
# .* matches any character (except newline) 0 or more times.
# (?=,) is a positive lookahead assertion that matches a comma (,) without including it in the match
# sub is used since some entries contain multiple columns

endangered_table$Location <- sub(".*(?=,)", "", endangered_table$Location, perl = TRUE)

# Step-5: Removing the comma
endangered_table$Location <- gsub(",", "", endangered_table$Location)


# Step-6: Trimming the whitespaces
endangered_table$Location <- str_trim(endangered_table$Location, side = "both")


# Step-7: Finding and fixing the exceptions
# In row 28 -Ken Kenya
# checking for index of any entries with repeating letters and fixing them
chk_index <- which(grepl("Ken", endangered_table$Location))
endangered_table$Location[chk_index] <- str_remove(endangered_table$Location[chk_index], "Ken")

# In row 32 -Serbia[a]
# checking for index of any entries with Serbia[a]
chk_index_2 <- which(grepl("Serbia", endangered_table$Location))
endangered_table$Location[chk_index_2] <- str_sub(endangered_table$Location[chk_index_2], end = -(nchar("[a]") + 1))


# In row 33- Côte d'Ivoire* Guinea*
# the above are 2 different country names so I will leave them both
chk_index_3 <- which(grepl("Guinea", endangered_table$Location))
endangered_table$Location[chk_index_3] <- "Côte d'Ivoire/ Guinea"

# In row 37- JerJerusalem(no nation named by UNESCO)[nb
chk_index_4 <- which(grepl("Jerusalem", endangered_table$Location))
endangered_table$Location[chk_index_4] <- "Israel"


# Step-8: Trimming the whitespaces again
endangered_table$Location <- str_trim(endangered_table$Location, side = "both")


# -----------------------------QUESTION 3---------------------------------------

# Q3 Using computational methods, split the variable that contains the criteria
# (“Criteria”) into two variables:
# “Type” (cultural/natural) and “Criteria” (containing roman numbers)

new_col <- list()

for (i in seq_along(endangered_table)) {
  # Split the 'criteria' column into 'type' and 'roman' columns using a colon as the separator
  new_col[[i]] <- str_split_fixed(endangered_table$Criteria[i], ":", 2)
  endangered_table$Type[i] <- new_col[[i]][, 1]
  endangered_table$roman[i] <- new_col[[i]][, 2]
}


# Out of loop-Remove the original column
endangered_table$Criteria <- NULL



# Out of loop- Rename roman column to criteria
colnames(endangered_table)[which(colnames(endangered_table) == "roman")] <- "Criteria"


# -----------------------------QUESTION 4---------------------------------------

# Q4 maintain only the data in acres and remove the hectares (ha) from the “Area”
# variable.

for (i in seq_along(endangered_table$`Areaha (acre)`)) {
  # replace missing values with NA
  endangered_table$`Areaha (acre)`[i] <- gsub("—", NA, endangered_table$`Areaha (acre)`[i])
  # keep only text within bracket which has acre values
  if (!is.na(endangered_table$`Areaha (acre)`[i])) {
    endangered_table$`Areaha (acre)`[i] <- sub("^[^(]*\\(", "(", endangered_table$`Areaha (acre)`[i])
    # remove brackets from the values
    endangered_table$`Areaha (acre)`[i] <- substring(endangered_table$`Areaha (acre)`[i], 2, nchar(endangered_table$`Areaha (acre)`[i]) - 1)
  }
}

endangered_table

# out of loop-change column name
names(endangered_table)[names(endangered_table) == "Areaha (acre)"] <- "Area(acre)"

# -----------------------------QUESTION 5---------------------------------------

# Q5 Using computational methods (Regex), clean the variable Endangered and maintain
# only the very last year

# remove values before comma
endangered_table$Endangered <- gsub("^[^,]*,", "", endangered_table$Endangered)
# remove the dash after the values
endangered_table$Endangered <- gsub("–", "", endangered_table$Endangered)
# remove empty spaces before the values
endangered_table$Endangered <- str_trim(endangered_table$Endangered, side = "left")


# -----------------------------QUESTION 6---------------------------------------

# Q6 Make sure that you have numeric vectors and characters vectors only

# checking class of each column
for (i in seq_along(endangered_table)) {
  print(class(endangered_table[[i]]))
}


# there are 3 columns which need to be numeric type-Area, Year,Endangered
for (i in c(3, 4, 5)) {
  endangered_table[[i]] <- as.numeric(endangered_table[[i]])
}


# checking class again of each column
for (i in seq_along(endangered_table)) {
  print(class(endangered_table[[i]]))
}



# ******************************  PART3 ****************************************
# -----------------------------QUESTION 1---------------------------------------

# Q1 What type of site (cultural or natural) is the most common in the endangered
# list and how many does each type of site have?

# counting the number for all cultural and natural sites

c_cultural <- count(endangered_table, Type)
print(c_cultural)

# checking the most common for all cultural and natural sites
which.max(c_cultural$n)
print(c_cultural[which.max(c_cultural$n), ])

# -----------------------------QUESTION 2---------------------------------------

# Q2 What site has the largest area (in m2 ) and what site has the smallest area

sqm_area <- list()
for (i in seq_along(endangered_table$`Area(acre)`)) {
  sqm_area[i] <- round((4046.86 * endangered_table$`Area(acre)`[i]), 2)
}
sqm_area
print(data.frame(endangered_table$Name[which.max(sqm_area)], sqm_area[which.max(sqm_area)]))

print(data.frame(endangered_table$Name[which.min(sqm_area)], sqm_area[which.min(sqm_area)]))


# -----------------------------QUESTION 3---------------------------------------

# Q3 What is the frequency (in years) with which sites were put on endangered
# list-plot

# Install and load ggplot2
install.packages("ggplot2")
library("ggplot2")


#create a frequency distribution table

bin_width <- 5
bins <- cut(endangered_table$Endangered, breaks = seq(min(endangered_table$Endangered), max(endangered_table$Endangered) + bin_width, bin_width), right = FALSE)

# Create a frequency distribution table
freq_table_en <- as.data.frame(table(bins))

# Rename the columns
colnames(freq_table_en) <- c("bin_interval2", "count2")


#make a bar plot from the frequency distribution table

ggplot(data = freq_table_en, aes(x = bin_interval2, y = count2)) +
  geom_bar(stat = "identity", col = "black", fill = "pink") +
  xlab("Year(every 5 year interval)") +
  scale_y_continuous(breaks=seq(0,24,by=3))+
  ylab("Count") +
  ggtitle("Frequency distribution of sites put on endangered list every 5 years")




# -----------------------------QUESTION 4---------------------------------------

# Q4 What country has more sites in the list? how many sites has each country?

# counting the number for all countries

c_country <- count(endangered_table, Location)
print(c_country, n = 34)

# checking the max count for country
which.max(c_country$n)
print(c_country[which.max(c_country$n), ])



# -----------------------------QUESTION 5---------------------------------------

# Q5 What country has more sites in the list?
# How long took each site to be in the endangered list?

time_taken <- data.frame("Site" = endangered_table$Name, "Time_taken(yrs)" = endangered_table$Endangered - endangered_table$`Year (WHS)`)

print(time_taken[which.max(time_taken$Time_taken.yrs.), ])



# -----------------------------QUESTION 6---------------------------------------

# Q6 What is the frequency with which sites were put on the endangered list after
# they were inscribed in the World Heritage List-plot


#create a frequency distribution table

bin_width <- 3
bins <- cut(time_taken$Time_taken.yrs., breaks = seq(min(time_taken$Time_taken.yrs.), max(time_taken$Time_taken.yrs.) + bin_width, bin_width), right = FALSE)

# Create a frequency distribution table
freq_table <- as.data.frame(table(bins))

# Rename the columns
colnames(freq_table) <- c("bin_interval", "count")


#make a bar plot from the frequency distribution table

ggplot(data = freq_table, aes(x = bin_interval, y = count)) +
  geom_bar(stat = "identity", col = "grey", fill = "yellow") +
  xlab("Time Interval(in years which are multiples of 3)") +
  scale_y_continuous(breaks=seq(0,17,by=1))+
  ylab("Count") +
  ggtitle("Frequency distribution of Time taken(in yrs) to become endangered after being a WHS")


# **********************************END*****************************************
