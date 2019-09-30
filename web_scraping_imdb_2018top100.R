#################################################################################################
# Web Scraping - IMDB 2018 Top 100 Movies                         
#
# Last Revision: August 2nd, 2019
#
# Author:
# Lee Ping Tay
#
#
# Description:
# This script is used to scrape the IMDb website for the Top 100 most popular feature films 
# released in 2018 using R
#
# Contents: 
# Libraries and Environment
# Data Import and Preprocessing
# Data Extraction from the website / Web Scraping
# Write to Excel
#
################################################################################################


################################################################################################
# Libraries and Environment
################################################################################################

library(tidyverse)    
library(rvest)       
library(XML)
library(xml2)
library(robotstxt)
library(writexl)


################################################################################################
# Data Import and Preprocessing
################################################################################################

# Check paths (robotstxt)
paths_allowed(paths="https://www.imdb.com/list/ls047677021/")
# www.imdb.com                      No encoding supplied: defaulting to UTF-8.                   
# TRUE


################################################################################################
# Data Extraction from the website / Web Scraping
################################################################################################

# Specify the url for the website to be scraped
url <- "https://www.imdb.com/list/ls047677021/"

# Read the HTML code from the website
webpage <- read_html(url)

# scrape  the following data from the IMDB website by using CSS Selectors

### Rank ###
rank <- webpage %>% 
        html_nodes('.text-primary') %>% 
        html_text() %>% 
        as.numeric()

length(rank)  # 100

### Title ###
title <- webpage %>% 
         html_nodes('.lister-item-header a') %>% 
         html_text()

length(title)  # 100

### Description ###
description <- webpage %>% 
               html_nodes('.lister-item-content p:nth-child(5)') %>% 
               html_text() %>% 
               str_replace_all("[\n]" , "") %>% 
               str_trim()

length(description)  # 100


### Runtime ###
runtime <- html_nodes(webpage, '.text-muted .runtime') %>% 
           html_text() %>% 
           str_extract('\\d+') %>% 
           as.numeric()

length(runtime)  # 100


### Genre ###
genre <- html_nodes(webpage, '.genre') %>% 
         html_text() %>% 
         str_replace_all("[\n]", "") %>% 
         str_trim() %>% 
         str_extract("[A-z]+") %>% 
         as.factor()

length(genre)  # 100

### Rating ###
rating <- html_nodes(webpage, ".ipl-rating-star.small .ipl-rating-star__rating") %>% 
          html_text() %>% 
          as.numeric()

length(rating)  # 100


### Metascore ###
metascore <- html_nodes(webpage, '.metascore') %>% 
             html_text() %>% 
             as.numeric()

length(metascore)  # 99

# Missing metascore data for movie 36
top <- metascore[1:35]
bottom <- metascore[36:length(metascore)]
metascore <- append(top,NA)
metascore <- append(metascore,bottom)

summary(metascore)

### Votes ###
votes <- html_nodes(webpage, '.lister-item-content p:nth-child(7) span:nth-child(2)') %>% 
         html_text() %>% 
         str_replace(",", "") %>% 
         as.numeric()

length(votes) # 99

# Missing votes data (45,939) for movie 36
top_votes <- votes[1:35]
bottom_votes <- votes[36:length(votes)]
votes <- append(top_votes,45939)
votes <- append(votes,bottom_votes)

summary(votes)


### Gross Earning in Mil
gross <- html_nodes(webpage, '.lister-item-content > p:nth-child(7) > span:nth-child(5)') %>% 
         html_text() %>% 
         str_replace("M", "") %>% 
         str_extract("\\d+\\.*+\\d+") %>%   # or ("[[:digit:]]+\\.*[[:digit:]]*") or ("\\(?[0-9.]+\\)?")
         as.numeric()

length(gross) # 90

#Filling missing entries with NA
for (i in c(35,36,41,42,44,73,76,86,98,99)){
  
  top_gross <- gross[1:(i-1)]
  bottom_gross <- gross[i:length(gross)]
  gross <- append(top_gross,NA)
  gross <- append(gross,bottom_gross)
  
}

summary(gross)


### Director ###
director <- html_nodes(webpage, '.lister-item-content p:nth-child(6) a:nth-child(1)') %>% 
            html_text() %>% 

length(director) # 99

# Director data (Vince Marcello) is missing for movie 36 as it is in .lister-item-content p:nth-child(5)

top_dir <- director[1:35]
bottom_dir <- director[36:length(director)]
director <- append(top_dir,"Vince Marcello")
director <- append(director,bottom_dir)

director <- director %>%  as.factor()


### Actor ###
actor <- html_nodes(webpage, '.lister-item-content .ghost+ a') %>% 
         html_text() %>% 
         as.factor()

# Create a data frame by combining all the data
top100movies_df <- data.frame(rank, title, description, runtime, genre, rating, metascore, votes, director, actor)

dim(top100movies_df)  # 100 10

################################################################################################
# Write to Excel (.xlsx) file
################################################################################################

# Write the combined data to the Excel file
write_xlsx(top100movies_df, "web_scraping/2018_top100movies.xlsx")
