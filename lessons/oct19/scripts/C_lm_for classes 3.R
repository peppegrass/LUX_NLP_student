#' Title: Linear modeling for classification
#' Purpose: Why doesn't LM work for binary classification?
#' Author: Ted Kwartler
<<<<<<< HEAD
#' email: edward.kwartler@hult.edu
=======
#' email: edwardkwartler@fas.harvard.edu
>>>>>>> 5239acfb293c063120a7eac91d25882503f325b6
#' License: GPL>=3
#' Date: Dec 28 2020
#'

# libs
library(ggplot2)
library(dplyr)

# wd
<<<<<<< HEAD
setwd("~/Documents/GitHub/LUX_NLP_student/lessons/oct19/data")
=======
setwd("~/Desktop/LUX_NLP_student/lessons/oct19/data")
>>>>>>> 5239acfb293c063120a7eac91d25882503f325b6

# Data
data('diamonds')

# Convert to binary
<<<<<<< HEAD
diamonds$icedOut <- ifelse(diamonds$price >= 11000,1, 0) #GG: creating a dummy
=======
diamonds$icedOut <- ifelse(diamonds$price >= 11000,1, 0)
>>>>>>> 5239acfb293c063120a7eac91d25882503f325b6
diamonds$price   <- NULL

set.seed(1234)
sampDiamonds <- sample_n(diamonds, 10000)

# Remember this?
p <- ggplot(sampDiamonds, aes(carat, icedOut)) + 
  geom_point(alpha = 0.2)
p

# Since we see a relationship let's make a linear model to predict prices
fit   <- lm(icedOut ~ carat + 0, sampDiamonds)
xBeta <- coefficients(fit)

# Add out model predictions; does this look like a good fit?
p <- p + geom_abline(intercept =  0, slope = xBeta, color='red')
<<<<<<< HEAD
p #GG: criticizing linear probability model, but because they are concerned about prediction here
=======
p
>>>>>>> 5239acfb293c063120a7eac91d25882503f325b6

# Suppose you *could* get a 12 carat diamond
hopeDiamond  <- data.frame(carat = 12.22)
gettingCrunk <- predict(fit, hopeDiamond)

# 1= Yes, the diamonds is more than $11k; 0 means no.  What does this mean?  We must have used the wrong method!
gettingCrunk

# End