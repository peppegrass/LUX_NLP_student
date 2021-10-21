#' Title: Webscraping multiple pages
#' Purpose: Scrape a page for all urls, then scrape each one in order
#' Author: Ted Kwartler
#' email: edward.kwartler@fas.harvard.edu
#' License: GPL>=3
#' Date: Oct 20, 2021
#' 
#GG: this sounds similar to the exercise about economists that we did with Nikos by scraping Repec
# Library
library(rvest)
library(stringi)

# wd
setwd("~/Documents/GitHub/LUX_NLP_student/lessons/oct21/data")

# Instructor Page
webpage <- 'http://www.gserm.ch/stgallen/instructors/'

# Get all links
getLinks <- read_html(webpage) %>% html_nodes(xpath='/html/body/div[1]/div/a') %>% #GG: this is not regex, it is xpath. it is a language related to html structure
  #html_nodes(".instructor") %>% #GG: ".instructor" is also CSS  #GG: selector gadget extension for chrome. useful, i.e. under header 3 (CSS node H3) OR (if not CSS), open up and inspect nodes in xpath structure
  html_attr('href')
getLinks
# GG: inspect -> xpath -> if you wanna use xpath you have to declare "html_nodes(xpath=)" -> right click in the HTML -> "copy full xpath"
# GG: selector gadget -> CSS

# Extract and clean names
instructorNames <- gsub('https://www.gserm.ch/stgallen/instructor/',
                        '', 
                        getLinks) 
instructorNames

instructorNames <- gsub("/", "", instructorNames)
instructorNames <- gsub("-", " ", instructorNames)
instructorNames <- stri_trans_totitle(instructorNames)
instructorNames

# Follow links to get more bio's
allBios <- list()
for (i in 1:length(instructorNames)){
  # Progress Msg
  cat(instructorNames[i])
  
  # Get the bio text
  x <- read_html(getLinks[i]) %>% 
    html_nodes(".text") %>% html_text()
  cat('...')
  # Get the photo URL
  y <- read_html(getLinks[i]) %>% 
    html_nodes(".instructor-photo") %>% html_attr("src")
  cat('...')
  df <- data.frame(instructorName = instructorNames[i],
                   bio            = x,
                   photo          = y)
  cat('complete\n')
  allBios[[i]] <- df
}

# Arrange the list to a single data frame
allBios     <- do.call(rbind, allBios)

# Drop the line returns 
allBios$bio <- gsub("[\r\n\t]", "", allBios$bio)
allBios[21,]

write.csv(allBios, 
          'allBios.csv',
          row.names = F)


# End