######################################################################
##AEDA Machine Learning class exercise - re-create Lucas 2020!      ##
##This is an abbreviated version of the scripts from Lucas 2020,    ##
##excluding phylogenetic correlation analyses and some              ##
##of the model diagnostics that take a long time to run.            ##
##Full script available at: https://doi.org/10.5281/zenodo.3840994. ##
######################################################################

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

#Data - Get the pantheria dataset from the traitdata package 

data(pantheria)
p<-pantheria

# Check the variable names, basic structure of the dataset, and see how much missing data there is for each variable

names(p)
sapply(p, function(x) mean(is.na(x))) %>% sort
sapply(p, class)
dim(p)


#Now we need to choose a variable of interest and make some basic exploratory plots.
#Want something with quite a lot of data. Litter size?

sum(!is.na(p$LitterSize))

#Basic plot and summary of litter size data. Note that it is not normally distributed! 

ggplot(p, aes(LitterSize)) + geom_histogram()
p$LitterSize %>% summary

## PLot litter size data by mammalian order, for orders with >40 species

large_orders <- 
  p %>% 
  filter(!is.na(sum(!is.na(p$LitterSize)))) %>% 
  group_by(Order) %>% 
  count() %>%
  arrange(desc(n)) %>% 
  filter(n > 40) %>% 
  pull(Order)

p %>% 
  filter(Order %in% large_orders) %>% 
  ggplot(aes(LitterSize)) + 
  geom_histogram() + 
  facet_wrap(~ Order)

## Boxplots by mamallian order (all)

ggplot(p, aes(x = Order, y = LitterSize)) + geom_boxplot()

## Boxplots by mammalian order (orders with >40 species)

p %>%
  group_by(Order) %>% 
  add_tally %>% 
  filter(n > 20) %>% 
  ggplot(aes(x = Order, y = LitterSize)) + geom_boxplot()

## Don't wish to do too many bivariate plots at this point. Going to do a priori variable selection and p values later.
## Let's just look at litter size vs population density
ggplot(p, aes(x = PopulationDensity_n.km2, y = LitterSize)) + 
  geom_point() + 
  geom_smooth() + 
  geom_smooth(method = 'lm', se = FALSE, colour = 'red') + 
  scale_x_log10()


## Some data cleaning
# Remove species where litter size = NA or less than one (doesn't make sense).
# Also rename the response variable as "y" and remove one variable and the references column

p <- p %>% 
  filter(!is.na(LitterSize)) %>% 
  filter(LitterSize >= 1) %>% 
  mutate(y = log1p(LitterSize)) %>% 
  dplyr::select(-LitterSize, -References, -TeatNumber)

## Impute missing data - use the median value for the whole dataset in each case

preprocesses <- preProcess(p, method = 'medianImpute')
p_impute <- predict(preprocesses, p)

## Data summary for dataset with imputation

dim(p_impute)
names(p_impute)

# Remove taxonomic data
cols=c(5:51,53)

p_impute_data=p_impute[,cols]

dim(p_impute_data)
names(p_impute_data)

## Plot distributions of each variable
## Which ones look like they need log transformation?

par(mfrow = c(2, 2))

for(i in 0:11){
  for( j in 1:4){
    
    if(j + 4 * i <= ncol(p_impute_data)){
      hist(p_impute_data[, j + 4 * i], breaks = 100, ylim = c(0, 80), main = j + 4 * i)
    }
    
  }
  print(i)
  par(mfrow = c(2, 2))
}

## Log Transforms
## for those variables where the distribution is non-normal

log_cols <- c(2, 4, 7, 8, 
              10, 11, 13, 14, 15, 17, 18, 19, 
              20, 21, 22, 23, 26, 27, 28, 29,  
              31, 32, 33, 
              40, 41, 42)

p_impute_data[, log_cols] <- log1p(p_impute_data[, log_cols])

## Check the transformed variable distributions

par(mfrow = c(2, 2))

for(i in 0:11){
  for( j in 1:4){
    
    if(j + 4 * i <= ncol(p_impute_data)){
      hist(p_impute_data[, j + 4 * i], breaks = 100, ylim = c(0, 80), main = j + 4 * i)
    }
    
  }
  print(i)
  par(mfrow = c(2, 2))
}

### Note - the "spike" in values in some of these plots is due to imputation!

## Now fit 4 models.

#caret_Setup

folds <- createFolds(p$y, k = 5, returnTrain = TRUE)
trcntrl <- trainControl(index = folds, savePredictions = TRUE, search = 'random')

#Base model: a linear model with a priori variable selection and no interactions 
#We will fit this using the "train" function from caret, which can fit traditional linear models as well as models incorporating machine learning
apriori_formula <- y ~ AdultBodyMass_g + AgeatFirstBirth_d + BasalMetRate_mLO2hr + GestationLen_d + LittersPerYear + MaxLongevity_m
mammal_m0_lm <- train(apriori_formula, data = p_impute_data, method = 'lm', trControl = trcntrl, na.action = na.omit)

plotCV(mammal_m0_lm)

mammal_m0_lm

summary(mammal_m0_lm$finalModel)

#Machine learning model I: elastic_net
#This is the "statistical, parametric" model from Lucas 2020
#Basically, we fit a linear model with all of the possible variables, then impose a "regularization" penalty on some coefficients

enet_gr <- expand.grid(lambda = 10 ^ seq(0, -4, length.out = 20), fraction = c(seq(0.01, 1, length.out = 25)))
mammal_m1_enet <- train(y ~ ., data = p_impute_data, method = 'enet', tuneGrid = enet_gr, trControl = trcntrl, na.action = na.omit)

#Cross-validation: plot observed vs predicted
plotCV(mammal_m1_enet)

#elastic_net_summary - best model fit
mammal_m1_enet$results$Rsquared %>% max

# Plot R2 vs regularization strength (recreate figure 1a)

mammal_m1_enet$results %>%
  ggplot(aes(fraction, Rsquared, colour = lambda, group = factor(lambda))) +
  geom_line() +
  geom_point() + scale_color_viridis_c(trans = 'log10') + xlab('Lasso/Ridge fraction')

#Machine Learning Model II: gaussian proces model
#This is the "nonparametric, statistical" model from Lucas 2020
#Now we are not assuming linear relationships!

gp_gr <- data.frame(sigma = c(0.01, 0.02, 0.04, 0.08, 0.16))
mammal_m2_gp <- train(y ~ ., data = p_impute_data, method = 'gaussprRadial', tuneGrid = gp_gr, trControl = trcntrl, na.action = na.omit)

## Plot R2 vs sigma (recreate figure 1b)

mammal_m2_gp$results %>% ggplot(aes(sigma, Rsquared)) +
  geom_line() + geom_point() + xlab('Sigma')

#Cross-validation: plot observed vs predicted
plotCV(mammal_m2_gp)

#gaussian process summary
mammal_m2_gp
mammal_m2_gp$results$Rsquared %>% max


#Machine Learning Model III: random forest
#This is the "nonparametric, nonstatistical" model from Lucas 2020
#Training this model can take a bit of time (~10 min)

rf_gr <- expand.grid(mtry = c(2, 5, 10, 20, 30), splitrule = 'variance', min.node.size = c(5, 10, 20, 50))
mammal_m3_rf <- train(y ~ ., data = p_impute_data, method = 'ranger', tuneGrid = rf_gr, trControl = trcntrl, na.action = na.omit, importance = 'impurity', num.trees = 1000)

##Plot # of random predictors and minimum node size vs R2 (recreate figure 1c)

mammal_m3_rf$results %>%
  ggplot(aes(mtry, Rsquared, colour = factor(min.node.size), group = factor(min.node.size))) +
  geom_line() +
  geom_point() +
  labs(colour = 'min.node.size')

#Cross-validation: plot observed vs predicted
plotCV(mammal_m3_rf)

# Random forest summary
mammal_m3_rf
mammal_m3_rf$results$Rsquared %>% max

#Compare model fits

compare_models(mammal_m2_gp, mammal_m3_rf)
compare_models(mammal_m1_enet, mammal_m3_rf)
compare_models(mammal_m0_lm, mammal_m3_rf)

# Variable importance
varImp(mammal_m1_enet)
varImp(mammal_m2_gp)
varImp(mammal_m3_rf)

## Finally, let's plot some functional forms

# partial dependence plot for gestation length (recreating figure 3)
partial(mammal_m1_enet,pred.var = c('GestationLen_d'), plot = TRUE)
partial(mammal_m2_gp, pred.var = c('GestationLen_d'), plot = TRUE)
partial(mammal_m3_rf, pred.var = c('GestationLen_d'), plot = TRUE)

# partial dependence plot for latitude
partial(mammal_m1_enet,pred.var = c('GR_MidRangeLat_dd'), plot = TRUE)
partial(mammal_m2_gp, pred.var = c('GR_MidRangeLat_dd'), plot = TRUE)
partial(mammal_m3_rf, pred.var = c('GR_MidRangeLat_dd'), plot = TRUE)

# For exploring two-way effects - these plots take a long time to generate so I am leaving them out of the in-class analysis
# 2-D partial dependence plot for gestation length and PET (recreating figure 5)
# partial(mammal_m1_enet,pred.var = c('GestationLen_d', 'PET_Mean_mm'),plot = TRUE)
# partial(mammal_m2_gp,pred.var = c('GestationLen_d', 'PET_Mean_mm'),plot = TRUE)
# partial(mammal_m3_rf,pred.var = c('GestationLen_d', 'PET_Mean_mm'),plot = TRUE)

## This last bit of code can be used to explore interaction strengths. 
## The plots take a long time to generate so I have left it out of the in-class analysis but you might want to try it out! 
## Requires one additional library (iml)
#library(iml)

#predictor_gp = Predictor$new(m2_gp, data = dplyr::select(p_impute_data, -y), y = p_impute$y)
#predictor_rf = Predictor$new(m3_rf, data = dplyr::select(p_impute_data, -y), y = p_impute$y)
#interact_gp = Interaction$new(predictor_gp)
#interact_gp$results %>% arrange(desc(.interaction)) %>% head
#plot(interact_gp)

#interact_rf = Interaction$new(predictor_rf)
#interact_rf$results %>% arrange(desc(.interaction)) %>% head
#plot(interact_rf)