#' Title: Multiple Supervised Methods 
#' Purpose: Explore the rtexttools package
#' Author: Ted Kwartler
#' email: edwardkwartler@fas.harvard.edu
#' License: GPL>=3
#' Date: Dec 29 2020
#' 

# Set the working directory
setwd("~/Documents/GitHub/LUX_NLP_student/lessons/oct20/data")

# Libs
library(tm)
library(RTextTools)
library(yardstick)

# Bring in our supporting functions
source('~/Documents/GitHub/LUX_NLP_student/lessons/Z_otherScripts/ZZZ_supportingFunctions.R')

# Options & Functions
options(stringsAsFactors = FALSE, scipen = 999)
Sys.setlocale('LC_ALL','C')

# Create custom stop words
stops <- c(stopwords('SMART'), 'diabetes', 'patient')

# Get data
diabetes <- read.csv('diabetes_subset_8500.csv')
txt <- paste(diabetes$diag_1_desc,diabetes$diag_2_desc,diabetes$diag_3_desc)

# Subset to avoid overfitting
set.seed(1234)
idx <- sample(1:length(txt), floor(length(txt)*.70))
train <- txt[idx]
test  <- txt[-idx]

# Clean, extract text and get into correct object
cleanTrain <- cleanCorpus(VCorpus(VectorSource(train)), stops)
cleanTrain <- data.frame(text = unlist(sapply(cleanTrain, `[`, "content")),
                       stringsAsFactors=F)

# This is not our original cleanMatrix function!
trainDTMm <- create_matrix(cleanTrain, language="english")

# Create the container
# trainSize; if you want to split within the single matrix but best practice is to bring it in separate to mimic really new data processing 
container <- create_container(matrix    = trainDTMm,
                              labels    = diabetes$readmitted[idx], #GG: [idx] subsets the training set
                              trainSize = 1:length(idx), 
                              virgin=FALSE)

# Build Models, can take ages for complex algos
#models <- train_models(container, algorithms=c("GLMNET","SVM")) #"SVM","SLDA","BOOSTING","BAGGING", "RF","GLMNET","TREE","NNET"
#saveRDS(models, 'rtexttools_models.rds')
models <- readRDS('rtexttools_models.rds') #GG: saved the model above (commented-out). Can pass multiple algorithms, we've done GLM and SVM: obtain two model objects. Can fit multiple models with one function call to identify the best model
#GG: but I haven't done any matrix matching here

# Score the original training data
results <- classify_models(container, models)
head(results)

# Append Actuals
results$actual <- diabetes$readmitted[idx]

# Confusion Matrix
table(results$GLMNET_LABEL, results$actual)
table(results$SVM_LABEL, results$actual)

# Accuracy GLMNET_LABEL
autoplot(conf_mat(table(results$GLMNET_LABEL, results$actual)))
accuracy(table(results$GLMNET_LABEL, results$actual))

# Accuracy SVM_LABEL
autoplot(conf_mat(table(results$SVM_LABEL, results$actual)))
accuracy(table(results$SVM_LABEL, results$actual))
#GG: now the problem is how do I handle new documents & content drift? That's painful
# Now let's apply the models to "new" documents
# Clean, extract text and get into correct object
cleanTest <- cleanCorpus(VCorpus(VectorSource(test)), stops)
cleanTest <- data.frame(text = unlist(sapply(cleanTest, `[`, "content")),
                       stringsAsFactors=F)

# You have to combine the matrices to the original to get the tokens joined
allDTM <- rbind(cleanTrain, cleanTest) #GG: in their workflow, they're rowbinding it to the training set, kinda painful
allDTMm <- create_matrix(allDTM, language="english")
containerTest <- create_container(matrix    = allDTMm, #GG: this is how they do matrix matching
                                  labels    = diabetes$readmitted[idx], #GG: gotta be very explicit about these 3 parameters: #1
                                  trainSize = 1:length(idx), #GG: #2
                                  testSize  = (length(idx)+1):8500, #GG: #3
                                  virgin=T)

#testFit <- train_models(containerTest, algorithms=c("GLMNET", "SVM"))
#saveRDS(testFit, 'rtexttools_testFit.rds')
testFit <-readRDS('rtexttools_testFit.rds')
resultsTest <- classify_models(containerTest, testFit) #GG: now scoring the 2500 of the validation set

# Append Actuals
resultsTest$actual <- diabetes$readmitted[-idx]

# Confusion Matrix
summary(resultsTest$GLMNET_PROB)
summary(resultsTest$SVM_PROB)
table(resultsTest$SVM_LABEL, resultsTest$actual)
accuracy(table(resultsTest$SVM_LABEL, resultsTest$actual))
table(resultsTest$GLMNET_LABEL, resultsTest$actual)
accuracy(table(resultsTest$GLMNET_LABEL, resultsTest$actual))

# End