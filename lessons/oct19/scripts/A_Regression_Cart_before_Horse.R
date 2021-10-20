#' Title: Regression
#' Purpose: Learn about a regression model
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
library(tidyverse) # this is a late class addition, the diamonds data set moved libraries.

# Data
data('diamonds') # No Working Directory needed, ggplot come with the diamonds data
set.seed(1234)

# This is a simple down-sample, not a partitioning schema.  
# There is a difference because you could resample and get the same rows. 
# When you partition you want to ensure no overlap of records.
sampDiamonds <- sample_n(diamonds, 10000)

# EDA
summary(sampDiamonds)

# Simple Relationship Carets to Price
p <- ggplot(sampDiamonds, aes(carat, price)) +geom_point(alpha=0.02)
p

# Since we see a relationship let's make a linear model to predict prices
<<<<<<< HEAD
fit <- lm(price ~ carat + 0, sampDiamonds) #GG: forccing origin to be 0
=======
fit <- lm(price ~ carat + 0, sampDiamonds)
>>>>>>> 5239acfb293c063120a7eac91d25882503f325b6
fit

# Add out model predictions
xBeta <- coefficients(fit)
p     <- p + geom_abline(intercept =  0, slope = xBeta, color='red')
p

# End
