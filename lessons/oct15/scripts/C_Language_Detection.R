#' Title: Language detection
#' Purpose: Use textcat to ID which language is used
#' Author: Ted Kwartler
#' email: edwardkwartler@fas.harvard.edu
#' License: GPL>=3
#' Date: Oct 13, 2021
#'

# Set the working directory
setwd("~/Documents/GitHub/LUX_NLP_student/lessons/oct15/data")

# Libs
library(textcat)
library(cld2) #Google, compact language detector

# Options & Functions
testing <- T
options(stringsAsFactors = FALSE)

# Data
if(testing == T){
  unknownLanguageOne <- read.csv("tweets_Haddad_Fernando.csv", 
                                 nrows = 100)
} else {
  unknownLanguageOne <- read.csv("tweets_Haddad_Fernando.csv")
  
}

# Example languages supported
t(t(names(TC_byte_profiles)))

# Categorize the language
txtLanguage <- textcat(unknownLanguageOne$text)

# Review; overall OK!
head(txtLanguage, 10)

# Problematic texts; perhaps cleaning or longer passages would help
unknownLanguageOne$text[3]
cat(unknownLanguageOne$text[3])
unknownLanguageOne$text[4]
unknownLanguageOne$text[9]

# Most frequent
table(txtLanguage)

# Using google compact language detector; 80 languages
text <- c("To be or not to be?", "Ce n'est pas grave.", "Nou breekt mijn klomp!")
detect_language(text)
detect_language_mixed(unknownLanguageOne$text)

# It can check mixed documents too; and webpages
# line by line
detect_language( #GG: cool parses a web page, but you gotta tell it to ignore plain text (HTML and stuff)
  url('http://www.un.org/ar/universal-declaration-human-rights/'), plain_text = FALSE)

# returns top 3 identified
detect_language_mixed(
  url('http://www.un.org/fr/universal-declaration-human-rights/'), plain_text = FALSE)
detect_language_mixed(
  url('https://www.hket.com/'), plain_text = FALSE)

# End