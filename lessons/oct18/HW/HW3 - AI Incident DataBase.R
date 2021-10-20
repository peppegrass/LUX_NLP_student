#' Title: Homework III
#' Purpose: Perform sentiment analysis and cluster analysis
#' Author: Giuseppe Grasso
#' email: giuseppegrasso.econ@gmail.com
#' License: GPL>=3
#' Date: Oct 20, 2021
#'

# Preliminaries

## Setting the working directory
setwd("~/Documents/GitHub/LUX_NLP_student/lessons/oct18/HW")

## options
options(scipen = 999, stringsAsFactors = F)

## Libraries
library(skmeans)
library(tidytext)
library(tm)
library(clue)
library(cluster)
library(wordcloud)
library(lexicon)
library(dplyr)
library(plyr)
library(radarchart)
library(ggplot2)
library(ggthemes)

# Custom Functions
source('~/Documents/GitHub/LUX_NLP_student/lessons/Z_otherScripts/ZZZ_supportingFunctions.R')

# Examine Raw Text
## incidents_dates.csv
incidents_dates <- read.csv('incident_dates.csv')
incidents_dates$incidents_dates_fmt <- as.POSIXct(incidents_dates$Incident.Date, format = "%m/%d/%y")
incidents_dates$Year <- format(as.Date(incidents_dates$incidents_dates_fmt, format="%d/%m/%y"),"%Y")
incidents_dates <- incidents_dates[,c("incident_ID", "Incident.Date", "Year", "Citation", "Report.Count")]
t(incidents_dates[1,])
## incidents.csv
incidents <- read.csv('incidents.csv')
t(incidents[1:14,])

inc <- full_join(incidents,incidents_dates, by="incident_ID")
View(inc)
# End
