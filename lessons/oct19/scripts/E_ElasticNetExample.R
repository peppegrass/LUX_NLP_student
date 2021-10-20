#' Title: Elastic net example
#' Purpose: Build an elastic net for classification 
#' Author: Ted Kwartler
#' email: edward.kwartler@hult.edu
#' License: GPL>=3
#' Date: Dec 28 2020
#'

#GG: he prefers Lasso to Ridge when RHS vars are text (word dummies) because you can shrink some coefficients to 0. Useful because text is noisy and there are many words with no signal at all that should be dropped
# Wd
<<<<<<< HEAD
setwd("~/Documents/GitHub/LUX_NLP_student/lessons/oct19/data")
=======
setwd("~/Desktop/LUX_NLP_student/lessons/oct19/data")
>>>>>>> 002fc7dfb17fbffb8527b52d73eee5b656055a5b

# Libs
library(text2vec)
library(caret)
library(tm)
library(glmnet)

# Custom cleaning function #GG: pre-processing
diagnosisClean<-function(xVec){
  xVec <- removePunctuation(xVec)
  xVec <- stripWhitespace(xVec)
  xVec <- tolower(xVec)
  return(xVec)
}

# Read
diabetes <- read.csv('diabetes_subset_8500.csv')

# Concantenate texts in 3 columns #GG: collapsing as single-agent records
diabetes$diagnosisText <- as.character(paste(diabetes$diag_1_desc,
                                             diabetes$diag_2_desc,
                                             diabetes$diag_3_desc, sep=' '))

### SAMPLE : Patritioning
idx              <- createDataPartition(diabetes$readmitted,p=.7,list=F) #GG: createDataPartition() is caret function. I want 70% proportion. idx is an index of what rows to select versus not
trainDiabetesTxt <- diabetes[idx,]
testDiabetesTxt  <- diabetes[-idx,]

### EXPLORE
head(trainDiabetesTxt$diagnosisText,2)

table(trainDiabetesTxt$readmitted)

### MODIFY
# 
trainDiabetesTxt$diagnosisText <- diagnosisClean(trainDiabetesTxt$diagnosisText)

# Initial iterator to make vocabulary
iterMaker <- itoken(trainDiabetesTxt$diagnosisText, 
                    preprocess_function = list(tolower), #GG: possible to pass in pre-processing functions
                    progressbar         = T)
textVocab <- create_vocabulary(iterMaker, stopwords=stopwords('SMART'))
head(textVocab)
tail(textVocab)
nrow(textVocab)

#prune vocab to make DTM smaller #GG: chopping off terms with less then 10 counts, and enforcing proportions trimming (no need to do both)
prunedtextVocab <- prune_vocabulary(textVocab,
                                    term_count_min = 10,
                                    doc_proportion_max = 0.5,
                                    doc_proportion_min = 0.001)
nrow(prunedtextVocab)

# Using the pruned vocabulary to declare the DTM vectors 
vectorizer <- vocab_vectorizer(prunedtextVocab)

# Take the vocabulary lexicon and the pruned text function to make a DTM #GG: created document-term matrix
diabetesDTM <- create_dtm(iterMaker, vectorizer)
dim(diabetesDTM)

# Default is TF but if you want TF-IDF
#idf         <- get_idf(diabetesDTM)
#diabetesDTM <- transform_tfidf(diabetesDTM,idf)

### MODEL(s)
#train text only model
textFit <- cv.glmnet(diabetesDTM,
                     y=as.factor(trainDiabetesTxt$readmitted),
                     alpha=0.9,
                     family='binomial',
                     type.measure='auc',
                     nfolds=5,
                     intercept=F)


# Examine
head(coefficients(textFit),10)

# Subset to impacting terms
bestTerms <- subset(as.matrix(coefficients(textFit)), 
                    as.matrix(coefficients(textFit)) !=0)
head(bestTerms)
nrow(bestTerms)
ncol(diabetesDTM)

# Make training predictions
trainingPreds <- predict(textFit, diabetesDTM, type = 'class')
confusionMatrix(as.factor(trainingPreds),
                as.factor(trainDiabetesTxt$readmitted))
# End
