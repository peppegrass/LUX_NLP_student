#' Title: Speech to Text
#' Purpose: Use an API to perform speech to text
#' Author: Ted Kwartler
#' email: edwardkwartler@fas.harvard.edu
#' License: GPL>=3
#' Date: Oct 20, 2021
#' googleLanguageR is a package to perform the some of the same functions you've learned
#' but there are other APIs
#' Refs: https://cran.r-project.org/web/packages/googleLanguageR/vignettes/setup.html
#' http://code.markedmondson.me/googleLanguageR/index.html
#' IT COSTS MONEY, SO BE CAREFUL #GG: !!! Also not super intuitive 9-10 documentation

# libs
library(googleLanguageR)

#wd
setwd("~/Documents/GitHub/LUX_NLP_student/lessons/oct21/data")

# Authenticate
#gl_auth('~/Documents/googleCreds/speech2txt-25cb48408ae7.json') #GG: google language authentication has to be pointed to the project json file


#### General NLP API
# Google NLP - Named Entity Analysis (R has this for free w/library openNLP) #GG: google would be better, but you gotta pay
# Google NLP - Part of Speech Tagging (R has this for free w/library UDpipe) #GG: same
# Google NLP - Sentiment (R has this for free w/multiple libs and approaches) #GG: same
# Google NLP - Document Tagging (R *could* do this as a multi-class problem) #GG: could do this but gotta build multiclass classification model (more painful)
texts     <- paste(readLines('C05791318.txt'), collapse = ' ') #GG: emails from Hillary Clinton that we saw in the other exercise
#nlpResult <- gl_nlp(texts) #GG: send data to google servers and get a result. you gotta pay each time so we're gonna save the heck out of it
#saveRDS(nlpResult,'nlpResult.rds')
nlpResult <-readRDS('nlpResult.rds')

# POS tagging & meta
nlpResult$sentences
nlpResult$tokens
nlpResult$language

# Sentiment
nlpResult$documentSentiment$score #GG: google says nothing about the accuracy

# Tagged Topic
nlpResult$classifyText

#### Google Translation API
text <- "Text Mining in Practice with R. It's the math of talking, your two favorite things! "

## translate British into Danish
<<<<<<< HEAD
#translatedTxt <- gl_translate(text, target = "da") #GG: google language translate
#saveRDS(translatedTxt, 'translatedTxt.rds')
=======
#translatedTxt <- gl_translate(text, target = "fr")
#saveRDS(translatedTxt, 'translatedTxt_FR.rds')
>>>>>>> 9a8299967a5a9d7bd03b4c7df4d4fe1b91ae20bd
translatedTxt <- readRDS('translatedTxt.rds')
translatedTxt$translatedText

#### Speech to Text
#http://www.voiptroubleshooter.com/open_speech/american.html
#speechToTxt <- gl_speech('trimmed.wav', sampleRateHertz = 8000)
#saveRDS(speechToTxt, 'speechToTxt.rds')
speechToTxt <- readRDS('speechToTxt.rds')

# Results
speechToTxt$transcript

# Timed Text
speechToTxt$timings

# Text to Audio;requires the text-to-speech api enabled #GG: other way around
gl_talk_player(gl_talk(text, 
               output = 'someAudio.wav',
               gender = 'FEMALE'))

# End