#######################################
##AEDA Machine Learning assignment  ##
##Based on scripts from Lucas 2020 ##
####################################

## If you don't have these loaded yet, here are the libraries you will need
## Load general data manipulation / plotting libraries
library(dplyr)
library(ggplot2)

# Load modeling libraries
library(caret)
library(ranger)
library(pdp)
library(traitdata)
library(kernlab)

## Load helper scripts from Lucas 2020 - these are on the Github site as ML_helpers.R
## You can download and then source from this file (change file path to the location on your computer)
source('[your.directory]/ML_helpers.R')

set.seed(100)

#caret_Setup

folds <- createFolds(p$y, k = 5, returnTrain = TRUE)
trcntrl <- trainControl(index = folds, savePredictions = TRUE, search = 'random')

## Now let's use similar methods to those we used in the exercise to evaluate covariates of litter/clutch size in reptiles!
## Load a larger dataset for amniotes

data(amniota)

amniote_data<-amniota

names(amniote_data)
dim(amniote_data)

sum(!is.na(amniote_data$litter_or_clutch_size_n))

#The trait names should be pretty self-explanatory. "svl" = snout-vent length 

#Q1: Write some code to clean the data.
#Rename the variable of interest to "y", log transform variable of interest and remove any taxa with missing litter/clutch size data.
#Then, retain only taxa in the class Reptilia and remove any variables with no data (all NA).

##Q2: Plot the distribution of log-transformed litter/clutch size in reptiles.
##Histogram or boxplot (or both if you want) are fine.
##Visualizing by order may be useful.

##Q3: Write a little more data-cleaning code!
##Impute missing data and remove taxonomic data, common name, and scientific name.

##Q4: Visualize the distributions for the predictor variables.
##Identify which variables look like they have a highly non-normal distribution.
##Log-transform these variables and visualize again.
##Which of the four models we will fit need the input variables to be log-transformed?

##Q5: Fit a linear model relating your response variable to some potential predictors.
##To make this similar to our model for mammals, use adult body mass, age to maturity for females, incubation length, litters/clutches per year, and maximum longevity.
##Visualize model fit and get R2.
##How does this model compare to the mammal model?

##Q6: Fit an elastic net to the data. Use the same hyperparameters used for the mammal dataset.
##Visualize model fit and get maximum R2.
##Plot R2 vs lasso/ridge fraction and strength of regularization (lambda).
##Does using the elastic net improve prediction relative to the linear model for this dataset?

##Q7: Fit a Gaussian process model to the data. Use the same range of sigma values used for the mammal dataset. 
##Visualize model fit and get R2.
##Plot R2 vs sigma. How does this plot compare to the plot from the mammal dataset?
##Overall, does the Gaussian process model perform better than the linear model?

##Q7: Train a random forest on the data. Note - use a lower maximum number of random predictors by setting mtry = c(2, 5, 10, 20).
##Visualize model fit and get R2.
##Plot R2 vs node size and number of random predictors.
##What does the node size selected indicate about the amount of noise in the model?
##What does the number of random predictors selected indicate about interaction depth?

##Q8: Overall, which model(s) perform best at predicting litter/clutch size, and which perform the worst?
##Compare this to the mammal analysis. What does this say about the universality of these methods?

##Q9: Evaluate variable importance for the elastic net, gaussian process, and random forest.
##Which variable is most important across models? 

##Q10: Plot functional forms for the relationship between litter/clutch size and the most important variable for the Gaussian Process and the random forest models.
##How do they differ?
##What does this say about the likely relation ship between litter/clutch size and the best predictor variable?

