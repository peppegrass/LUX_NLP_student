#' Title: 538 Website API
#' Purpose: Review  numeric API
#' Author: Ted Kwartler
#' email: edwardkwartler@fas.harvard.edu
#' License: GPL>=3
#' Date: June 17, 2021
#'
#GG: API better than webscraping. because when you build script for webscraping you have to customize script for each page
#GG: information in json from API are much cleaner. You get the numerics you need more neatly arranged. You get to the data more easily
# Libraries
library(jsonlite) #GG: make request
library(xts) #GG: timeseries lib
library(zoo) #GG: timeseries lib
library(dygraphs) #GG: graphics
library(lubridate) #GG: working with dates

# Original pages
# https://projects.fivethirtyeight.com/trump-approval-ratings/
# https://projects.fivethirtyeight.com/biden-approval-rating/

# Developer Tab has two API endpoints
historicalURL <- 'https://projects.fivethirtyeight.com/biden-approval-rating/historical-approval.json' #GG: does not have expiration date. static data. contrast with line 22 URL of "A_youtubeAPI_example.R". "&expire" is the parameter for the expiration date

presURL <- 'https://projects.fivethirtyeight.com/biden-approval-rating/approval.json'
# Table Info
#tableURL <-  'https://projects.fivethirtyeight.com/biden-approval-rating/polls.json'

# Get Historical
approvalRatings <- fromJSON(historicalURL)
head(approvalRatings)

# Get President
presApproval   <- fromJSON(presURL)
head(presApproval)
tail(presApproval)

# Subset to "All polls", and not future predictions & just estimates
subSurvey <- subset(presApproval, 
                    presApproval$subgroup == 'All polls' & 
                      presApproval$future == F)
disapprove <- ts(subSurvey$disapprove_estimate, 
                 start = c(2021, 23), 
                 frequency = 365)
approve    <- ts(subSurvey$approve_estimate, 
                 start = c(2021, 23), 
                 frequency = 365)

ratings <- cbind(disapprove, approve)
ratings <- as.zoo(ratings)
ratings <- as.xts(ratings, date_decimal(index(ratings)))

dygraph(ratings, "Biden Approval") %>%
  dySeries("approve", label = "approve", color = 'green') %>%
  dySeries("disapprove", label = "disapprove", color = 'red') %>%
  dyRangeSelector()

# End
