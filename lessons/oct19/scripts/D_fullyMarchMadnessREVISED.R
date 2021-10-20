#' Title: Fully March Madness Revised
#' Purpose: apply a logistic regression to basketball data
#' Author: Ted Kwartler
<<<<<<< HEAD
#' email: edward.kwartler@hult.edu
=======
#' email: edwardkwartler@fas.harvard.edu
>>>>>>> 5239acfb293c063120a7eac91d25882503f325b6
#' License: GPL>=3
#' Date: Dec 28 2020
#'

# Libs
library(vtreat)
library(MLmetrics)
library(pROC)
library(ggplot2)

# wd
<<<<<<< HEAD
setwd("~/Documents/GitHub/LUX_NLP_student/lessons/oct19/data")
=======
setwd("/Users/edwardkwartler/Desktop/LUX_NLP_student/lessons/oct19/data")
>>>>>>> 5239acfb293c063120a7eac91d25882503f325b6

# Data
bball <- read.csv('ncaa.csv')

# Idenfity the informative and target
names(bball)
targetVar       <- names(bball)[51]
informativeVars <- names(bball)[3:47]

# Review to get familiar
head(bball[,1:7])
head(bball[,47:51])

# Design a "C"ategorical variable plan 
plan <- designTreatmentsC(bball, 
                          informativeVars,
                          targetVar, 1)

# Apply to xVars
<<<<<<< HEAD
treatedX <- prepare(plan, bball) #GG: got warning because did not split data

# Fit a logistic regression model
fit <- glm(R1.Class.1.win ~., data = treatedX, family ='binomial') #GG: binomial is logistic in this jargon
=======
treatedX <- prepare(plan, bball)

# Fit a logistic regression model
fit <- glm(R1.Class.1.win ~., data = treatedX, family ='binomial')
>>>>>>> 5239acfb293c063120a7eac91d25882503f325b6
summary(fit)

# Backward Variable selection to reduce chances of multi-colinearity
# Takes 2m  to run so load a pre-saved copy that I already made 
#bestFit <- step(fit, direction='backward')
#saveRDS(bestFit, 'bestFitNCAA.rds')
bestFit <-readRDS('bestFitNCAA.rds')
summary(bestFit)

# Compare model size
length(coefficients(fit))
length(coefficients(bestFit))

# Get predictions
<<<<<<< HEAD
teamPreds <- predict(bestFit, type='response') #GG: to get predictions instead of log odds
=======
teamPreds <- predict(bestFit, type='response')
>>>>>>> 5239acfb293c063120a7eac91d25882503f325b6
head(teamPreds)

# Classify 
cutoff <- 0.5
teamClasses <- ifelse(teamPreds >= cutoff, 1,0)

# Organize w/Actual
results <- data.frame(Name                = bball$Name,
                      year                = bball$Year,
                      seed                = bball$Seeds,
                      actual              = bball$R1.Class.1.win,
                      ModelClassification = teamClasses)
head(results,12)

<<<<<<< HEAD
# Get a confusion matrix #GG: to sort of visualize quality of prediction (false positives & false negatives)
=======
# Get a confusion matrix
>>>>>>> 5239acfb293c063120a7eac91d25882503f325b6
(confMat <- ConfusionMatrix(results$ModelClassification, results$actual))

# What is the accuracy?
sum(diag(confMat)) / sum(confMat)

# Easier
Accuracy(results$ModelClassification,results$actual)

# Visually how well did we separate our classes?
ggplot(results, aes(x=teamPreds, color=as.factor(actual))) +
  geom_density() + 
  geom_vline(aes(xintercept = cutoff), color = 'green')


# End
