#' Title: Webscraping a single page
#' Purpose: Scrape a single page example
#' Author: Ted Kwartler
#' email: edwardkwartler@fas.harvard.edu
#' License: GPL>=3
#' Date: Oct 20, 2021
#'

# libraries
library(rvest) #GG: R data harvesting package. Python's is beautifulsoup
# GG: webscraping: extracting info from webpage (without requesting them via an API basically). If you're trying to block you should accept. FB doesn't allow by their terms of services to scrape them. Respect terms of service. Grey area.
# GG: everything you see (on your client side) can be scraped.
# GG: websites changes, so the scraping code needs to be updated all the time. On the other hand, JSON info more stable. Websites change more on the front-end (user experience) and less on the back-end (API)
# GG: always check for API first!
# Get the webpage #GG: you wanna use the "Elements" developer tab in chrome
movieURL <- 'https://www.imdb.com/title/tt0058331'
movie <- read_html(movieURL) #GG: grabbing the HTML
movie

# Numeric info
rating <- movie %>% 
  html_nodes(xpath = '/html/body/div[2]/main/div/section[1]/section/div[3]/section/section/div[3]/div[2]/div[1]/div[2]/div/div[1]/a/div/div/div[2]/div[1]/span[1]') %>%
  html_text() %>%
  as.numeric()
rating

# _somtimes_ helpful to see all nodes
castURL <- paste0(movieURL,'/fullcredits') #GG: adding "/fullcredits/" to the URL
# https://www.imdb.com/title/tt0058331/fullcredits
castURL %>%
  read_html() %>%
  html_nodes("*") %>% 
  html_attr("class") %>% 
  unique()

# Get the cast names
cast <- castURL %>%
  read_html() %>%
  html_nodes('#fullcredits_content') %>%
  html_nodes("tr") %>% #GG: extracting the table rows
  html_text()
cast
#GG: keep scrolling around with the "Elements" tab open and hover on the elements you are interested in to see where they are in the HTML
# Webscraping is messy!
cast  <- gsub("[\r\n]", "", cast)
cast  <- lapply(cast, trimws)
cast  <- unlist(cast)
cast2 <- strsplit(cast, '[...]') #GG: coz ellipses (...) is a column breaker
part  <- lapply(cast2, tail, 1) %>% unlist() %>% trimws()
actor <- lapply(cast2, head, 1) %>% unlist() %>% trimws()

df <- data.frame(part, actor)
head(df,12) #looks like first 5 rows are wrong
df <- df[-(1:5),]
head(df,12)

# What is the URL for the movie poster?
allURLS <- movie %>% html_nodes("a") %>% html_attr("href")
allURLS
mediaURLS <- allURLS[ grep('mediaviewer',allURLS, ignore.case = T)]
mediaURLS[1]
movieURL
gsub('/title/tt0058331','',mediaURLS[1])
postURL <- paste0(movieURL,gsub('/title/tt0058331','',mediaURLS[1]))
postURL

# Storyline #GG: storyline section
storyline <- movie %>%
  html_nodes(xpath = '/html/body/div[2]/main/div/section[1]/div/section/div/div[1]/section[6]/div[2]/div[1]/div[1]/div/text()') %>%
  html_text() 
storyline

# More mess!
movieGross <- movie %>%
  html_nodes(xpath = '/html/body/div[2]/main/div/section[1]/div/section/div/div[1]/section[10]/div[2]/ul/li[2]') %>% html_text()
movieGross

movieGross <- gsub('Gross US & Canada|[$]|,','',movieGross)
movieGross <- as.numeric(movieGross)
movieGross
# End

