#' Title: LSA for Modeling - Bayes
#' Purpose: use LSA to reduce dimensions and create a Bayesian model
#' Author: Ted Kwartler
#' email: edwardkwartler@fas.harvard.edu
#' License: GPL>=3
#' Date: Dec 28 2020
#' Other Options worth exploring
#' https://cran.r-project.org/web/packages/textmineR/vignettes/c_topic_modeling.html
#' https://cran.r-project.org/web/packages/RTextTools/index.html

# Set the working directory
<<<<<<< HEAD:lessons/oct20/scripts/C_lsa_for modeling_Bayesian.R
setwd("~/Documents/GitHub/LUX_NLP_student/lessons/oct20/data")
=======
setwd("~/Desktop/LUX_NLP_student/lessons/oct20/data")
>>>>>>> 002fc7dfb17fbffb8527b52d73eee5b656055a5b:lessons/oct20/scripts/B_lsa_for modeling_Bayesian.R

# Libs
library(tm)
library(lsa)
library(e1071)
library(yardstick)
library(ggplot2)

# Bring in our supporting functions
<<<<<<< HEAD:lessons/oct20/scripts/C_lsa_for modeling_Bayesian.R
source('~/Documents/GitHub/LUX_NLP_student/lessons/Z_otherScripts/ZZZ_supportingFunctions.R')
=======
source('~/Desktop/LUX_NLP_student/lessons/Z_otherScripts/ZZZ_supportingFunctions.R')
>>>>>>> 002fc7dfb17fbffb8527b52d73eee5b656055a5b:lessons/oct20/scripts/B_lsa_for modeling_Bayesian.R

# Options & Functions
options(stringsAsFactors = FALSE, scipen = 999)
Sys.setlocale('LC_ALL','C')

# Create custom stop words
stops <- c(stopwords('SMART'), 'car', 'electronic')

# Bring in some data
<<<<<<< HEAD:lessons/oct20/scripts/C_lsa_for modeling_Bayesian.R
carCorp <- VCorpus(DirSource("~/Documents/GitHub/LUX_NLP_student/lessons/oct20/data/AutoAndElectronics/rec.autos"))
electronicCorp <- VCorpus(DirSource("~/Documents/GitHub/LUX_NLP_student/lessons/oct20/data/AutoAndElectronics/sci.electronics"))
=======
carCorp <- VCorpus(DirSource("~/Desktop/LUX_NLP_student/lessons/oct20/data/AutoAndElectronics/rec.autos"))
electronicCorp <- VCorpus(DirSource("~/Desktop/LUX_NLP_student/lessons/oct20/data/AutoAndElectronics/sci.electronics"))
>>>>>>> 002fc7dfb17fbffb8527b52d73eee5b656055a5b:lessons/oct20/scripts/B_lsa_for modeling_Bayesian.R

# Clean each one
carCorp        <- cleanCorpus(carCorp, stops)
electronicCorp <- cleanCorpus(electronicCorp, stops)

# Combine
allPosts <-  c(carCorp,electronicCorp)
rm(carCorp)
rm(electronicCorp)
gc()

# Construct the Target
yTarget <- c(rep(1,1000), rep(0,1000)) #1= about cars, 0 = electronics

# Make TDM; lsa docs save DTM w/"documents in colums, terms in rows and occurrence frequencies in the cells."!
allTDM <- TermDocumentMatrix(allPosts, 
                             control = list(weighting = weightTfIdf))
allTDM

# Get 20 latent topics
##### Takes awhile, may crash small computers, so saved a copy
#lsaTDM <- lsa(allTDM, 20)
#saveRDS(lsaTDM, 'lsaTDM_tfidf.rds') 
lsaTDM <- readRDS('lsaTDM_tfidf.rds')

# Extract the document LSA values
docVectors <- as.data.frame(lsaTDM$dk)
head(docVectors)

# Append the target
docVectors$yTarget <- as.factor(yTarget)

# Sample (avoid overfitting)
set.seed(1234)
idx <- sample(1:nrow(docVectors),.6*nrow(docVectors))
training   <- docVectors[idx,]
validation <- docVectors[-idx,]

# Fit the Bayesian Model
fit <- naiveBayes(yTarget ~ ., data = training)

# Training Preds
pred <- predict(fit, training)
table(pred, training$yTarget)

# Validation Preds
pred <- predict(fit, validation)
(confMat <-table(pred, validation$yTarget))

# Simple model evals
summary(conf_mat(confMat))
autoplot(conf_mat(confMat))

# End