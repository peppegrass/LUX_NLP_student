#' Title: Elastic net example
#' Purpose: Build an elastic net for classification 
#' Author: Ted Kwartler
<<<<<<< HEAD
#' email: edward.kwartler@hult.edu
=======
#' email: edwardkwartler@fas.harvard.edu
>>>>>>> 5239acfb293c063120a7eac91d25882503f325b6
#' License: GPL>=3
#' Date: Dec 28 2020
#'

<<<<<<< HEAD
#GG: he prefers Lasso to Ridge when RHS vars are text (word dummies) because you can shrink some coefficients to 0. Useful because text is noisy and there are many words with no signal at all that should be dropped
# Wd
setwd("~/Documents/GitHub/LUX_NLP_student/lessons/oct19/data")
=======

# Wd
setwd("/Users/edwardkwartler/Desktop/LUX_NLP_student/lessons/oct19/data")
>>>>>>> 5239acfb293c063120a7eac91d25882503f325b6

# Libs
library(text2vec)
library(caret)
library(tm)
library(glmnet)

<<<<<<< HEAD
# Custom cleaning function #GG: pre-processing
=======
# Custom cleaning function
>>>>>>> 5239acfb293c063120a7eac91d25882503f325b6
diagnosisClean<-function(xVec){
  xVec <- removePunctuation(xVec)
  xVec <- stripWhitespace(xVec)
  xVec <- tolower(xVec)
  return(xVec)
}

# Read
diabetes <- read.csv('diabetes_subset_8500.csv')

<<<<<<< HEAD
# Concantenate texts in 3 columns #GG: collapsing as single-agent records
=======
# Concantenate texts in 3 columns
>>>>>>> 5239acfb293c063120a7eac91d25882503f325b6
diabetes$diagnosisText <- as.character(paste(diabetes$diag_1_desc,
                                             diabetes$diag_2_desc,
                                             diabetes$diag_3_desc, sep=' '))

### SAMPLE : Patritioning
<<<<<<< HEAD
idx              <- createDataPartition(diabetes$readmitted,p=.7,list=F) #GG: createDataPartition() is caret function. I want 70% proportion. idx is an index of what rows to select versus not
=======
idx              <- createDataPartition(diabetes$readmitted,p=.7,list=F)
>>>>>>> 5239acfb293c063120a7eac91d25882503f325b6
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
<<<<<<< HEAD
                    preprocess_function = list(tolower), #GG: possible to pass in pre-processing functions
=======
                    preprocess_function = list(tolower), 
>>>>>>> 5239acfb293c063120a7eac91d25882503f325b6
                    progressbar         = T)
textVocab <- create_vocabulary(iterMaker, stopwords=stopwords('SMART'))
head(textVocab)
tail(textVocab)
nrow(textVocab)

<<<<<<< HEAD
#prune vocab to make DTM smaller #GG: chopping off terms with less then 10 counts, and enforcing proportions trimming (no need to do both)
=======
#prune vocab to make DTM smaller
>>>>>>> 5239acfb293c063120a7eac91d25882503f325b6
prunedtextVocab <- prune_vocabulary(textVocab,
                                    term_count_min = 10,
                                    doc_proportion_max = 0.5,
                                    doc_proportion_min = 0.001)
nrow(prunedtextVocab)

# Using the pruned vocabulary to declare the DTM vectors 
vectorizer <- vocab_vectorizer(prunedtextVocab)

<<<<<<< HEAD
# Take the vocabulary lexicon and the pruned text function to make a DTM #GG: created document-term matrix
=======
# Take the vocabulary lexicon and the pruned text function to make a DTM 
>>>>>>> 5239acfb293c063120a7eac91d25882503f325b6
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
