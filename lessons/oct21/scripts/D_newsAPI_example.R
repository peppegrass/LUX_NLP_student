#' Title: News API
#' Purpose: Get data from JSON
#' Author: Ted Kwartler
#' email: edwardkwartler@fas.harvard.edu
#' License: GPL>=3
#' Date: Oct 20, 2021
#'
# GG: you need to get you API key (paid). Otherwise you can make 100 request per days for free (still get API)
# Libraries
library(jsonlite)
library(pbapply)

# Options
options(stringsAsFactors = F)

# www.newsapi.org Key
#apiKey <- readLines('~/Desktop/GSERM_Text_Remote_admin/newsAPI.txt')
#apiKey <- 'XXXXXXXXXXXXXXXXXXXXXXX' #GG: that's where he would put his key. Not a good idea to put API key in a script. Rather use a script and load it from there

# Top headlines in the US endpoint:
usURL <- paste0('https://newsapi.org/v2/top-headlines?country=us&apiKey=', apiKey) # paste0 is paste without the default 0s
usURL

# Endpoint for all news sources
#https://newsapi.org/v2/sources?apiKey=####################

# Get last weeks information
to   <- Sys.Date() #GG: today
from <- to-7 # today minus 7

# Let's get Al Jazeera Top Headlines
apiURL <- paste0('https://newsapi.org/v2/top-headlines?sources=',
                'al-jazeera-english', # get from the sources endpoint
                '&from=', from,
                '&',
                'to=', to,
                '1&sortBy=popularity&apiKey=',
                apiKey)
apiURL #GG: make URL

# Let's get the text
newsInfo <- fromJSON(apiURL)

# Organize the API response and save
newsInfo$status           <- NULL #GG: if status is ok I don't need this
newsInfo$totalResults     <- NULL #GG: don't need this
newsInfo$articles$source  <- NULL #GG: don't need this coz I know the source

finalContent <- newsInfo[[1]]
finalContent
#write.csv(finalContent, 'finalNewsContent.csv', row.names = F)

# 
source <- 'wsj.com' #GG: no restriction about dates or search terms (he said "queue" or "q")
allArticles <- fromJSON(paste0('https://newsapi.org/v2/everything?domains=',
                        source,
                        '&apiKey=b6d9f96c78b34ee98bd84a23d3f74bfb'))

allArticles$status
allArticles$totalResults
allArticles$articles[1,]
# End
