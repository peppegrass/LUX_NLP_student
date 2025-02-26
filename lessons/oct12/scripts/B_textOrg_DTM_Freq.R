#' Title: Text Organization for Bag of Words
#' Purpose: Learn some basic cleaning functions & term frequency
#' Author: Ted Kwartler
#' email: edwardkwartler@fas.harvard.edu
#' License: GPL>=3
#' Date: Oct 11, 2021
#'

# Set the working directory
setwd("~/Documents/GitHub/LUX_NLP_student/lessons/oct12/data")

# Libs
library(tm)

# Options & Functions
options(stringsAsFactors = FALSE)
Sys.setlocale('LC_ALL','C')

tryTolower <- function(x){
  # return NA when there is an error
  y = NA
  # tryCatch error
  try_error = tryCatch(tolower(x), error = function(e) e)
  # if not an error
  if (!inherits(try_error, 'error'))
    y = tolower(x)
  return(y)
}

cleanCorpus<-function(corpus, customStopwords){
  corpus <- tm_map(corpus, content_transformer(qdapRegex::rm_url)) 
  corpus <- tm_map(corpus, content_transformer(tryTolower))
  corpus <- tm_map(corpus, removeWords, customStopwords)
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, removeNumbers)
  corpus <- tm_map(corpus, stripWhitespace)
  return(corpus)
}

# Create custom stop words
stops <- c(stopwords('english'), 'lol', 'smh')

# Data
text <- read.csv('coffee.csv', header=TRUE)
#View(text)

# As of tm version 0.7-3 tabular was deprecated
names(text)[1] <- 'doc_id' #first 2 columns must be 'doc_id' & 'text' #GG: need to rename first column as 'doc_id' otherwise package won't like it

# Make a volatile corpus
txtCorpus <- VCorpus(DataframeSource(text))

# Preprocess the corpus
txtCorpus <- cleanCorpus(txtCorpus, stops)

# Check Meta Data; brackets matter!!
txtCorpus[[4]]
meta(txtCorpus[[4]]) #double [[...]]
t(meta(txtCorpus[4])) #single [...]

content(txtCorpus[4]) #single [...]
content(txtCorpus[[4]]) #double [[...]]

# Need to plain text cleaned copy? Saves time on large corpora
df <- data.frame(text = unlist(sapply(txtCorpus, `[`, "content")),
                 stringsAsFactors=F)

# Or use lapply
cleanText <- lapply(txtCorpus, content)
cleanText <- do.call(rbind, cleanText)

# Compare a single tweet
text$text[4]
df[4,]
cleanText[4]

# Make a Document Term Matrix or Term Document Matrix depending on analysis
txtDtm  <- DocumentTermMatrix(txtCorpus)
txtTdm  <- TermDocumentMatrix(txtCorpus)
txtDtmM <- as.matrix(txtDtm)
txtTdmM <- as.matrix(txtTdm)

# Examine
txtDtmM[1:4,1749:1751]
txtTdmM[1749:1751,1:4]


#### Go back to PPT ####

# Get the most frequent terms
topTermsA <- colSums(txtDtmM)
topTermsB <- rowSums(txtTdmM)

# Add the terms
topTermsA <- data.frame(terms = colnames(txtDtmM), freq = topTermsA)
topTermsB <- data.frame(terms = rownames(txtTdmM), freq = topTermsB)

# Review
head(topTermsA)
head(topTermsB)

# Which term is the most frequent?
idx <- which.max(topTermsA$freq)
topTermsA[idx, ] #GG: if you put 'word' inside "stops <- c(stopwords('english'), 'lol', 'smh')", you'll find "like" is the next most common word

# End
