#title: "Prediction of Concrete Compressive Strength"
#author: "Morgan Houston"
#dataset from http://archive.ics.uci.edu/ml/datasets/concrete+compressive+strength

#--------------------------
#Environment Setup
#--------------------------

#load required packages
#if (!require(package)) install.packages('package')
#library(package)
if (!require(tidyverse)) install.packages('tidyverse')
if (!require(readxl)) install.packages('readxl')
if (!require(here)) install.packages('here')
library(tidyverse)
library(readxl)
library(here)

#use the here package to avoid absolute file paths
data_file <- here('data','Concrete_data.xls')

#import data file and read into data frame
concrete <- read_excel(data_file)
concrete <- as.data.frame(concrete)
head(concrete)
nrow(concrete)
ncol(concrete)

#--------------------------------------
#Data Exploration
#--------------------------------------
#summary function to check basic metrics and if there are any missing values
summary(concrete)

#rename columns for convenience
concrete <- concrete %>%rename(
  cement = `Cement (component 1)(kg in a m^3 mixture)`,
  blast_furnace_slag = `Blast Furnace Slag (component 2)(kg in a m^3 mixture)`,
  fly_ash = `Fly Ash (component 3)(kg in a m^3 mixture)`,
  water = `Water  (component 4)(kg in a m^3 mixture)`,
  superplasticizer = `Superplasticizer (component 5)(kg in a m^3 mixture)`,
  coarse_aggregate = `Coarse Aggregate  (component 6)(kg in a m^3 mixture)`,
  fine_aggregate = `Fine Aggregate (component 7)(kg in a m^3 mixture)`,
  age = `Age (day)`,
  compressive_strength = `Concrete compressive strength(MPa, megapascals)`)

#plot distribution of each variable
hist(concrete$cement)
hist(concrete$blast_furnace_slag)
hist(concrete$fly_ash)
hist(concrete$water)
hist(concrete$superplasticizer)
hist(concrete$coarse_aggregate)
hist(concrete$fine_aggregate)
hist(concrete$age)

#plot distribution of concrete compressive strength
concrete %>%count(compressive_strength) %>% ggplot(aes(compressive_strength))+geom_histogram(bins = 50)

#plot distribution concrete strength without age < 28 days
concrete%>%filter(age> 27)%>%count(compressive_strength) %>% ggplot(aes(compressive_strength))+geom_histogram(bins = 50)

#compressive strength standards are 2500 - 5k psi
#https://cor-tuf.com/everything-you-need-to-know-about-concrete-strength/
#approx 17 to 35 mPa

#plot the correlation matrix to identify any highly correlated variables
if (!require(corrplot)) install.packages('corrplot')
library(corrplot)
C <- cor(concrete)
corrplot(C,method = 'number')
#save correlation plot to file for use in report
png(height=750, width=750, file="plots/corrplot.png")
corrplot(C,method = 'number')
dev.off()
#we see a correlation between superplasticizer and water, cement and compressive strength, w_c_ratio and compressive_strength (makes sense)

#examine some relationships based on correlation matrix
#plot water + superplasticizer
concrete %>% ggplot(aes(x=water,y=superplasticizer))+geom_point()
#plot water + cement
concrete %>% ggplot(aes(x=water,y=cement))+geom_point()
#plot cement + fly ash
concrete %>% ggplot(aes(x=cement,y=fly_ash))+geom_point()
#plot cement + blast_furnace slag
concrete %>% ggplot(aes(x=cement,y=blast_furnace_slag))+geom_point()
#plot blast_furnace_slag + fly_ash
concrete %>% ggplot(aes(x=blast_furnace_slag,y=fly_ash))+geom_point()
#plot aggregates
concrete %>% ggplot(aes(x=coarse_aggregate,y=fine_aggregate))+geom_point()


#plot distribution of age
concrete %>%count(age)%>%ggplot(aes(x=age,y=n))+geom_point()
#industry standard is testing concrete strength at 28 days
#also see high points at 3 days and 7 days, which are typical standard testing points for early measures of strength


#water cement ratio - Abrams 1918
#ratio of water weight to cement weight (w/c)
concrete <- concrete%>%mutate(w_c_ratio = (water/cement)) 
#calculate water to binder ratio
concrete <- concrete %>% mutate(additions = (blast_furnace_slag + fly_ash))
concrete <- concrete %>% mutate(binder = (cement + additions))
concrete <- concrete%>%mutate(w_b_ratio = (water/(cement + blast_furnace_slag + fly_ash))) #also explore the water to binder ratio
summary(concrete)

#plot water to binder relationship
concrete %>% ggplot(aes(x=water,y=binder)) +geom_point()

#plot w_c_ratio to compressive strength
concrete %>% ggplot(aes(x=w_c_ratio,y=compressive_strength))+geom_point()

#plot w_b_ratio to compressive strength
concrete %>% ggplot(aes(x=w_b_ratio,y=compressive_strength))+geom_point()

#we use the nearZeroVar function to identify near zero-variance variables
nearZeroVar(x=concrete,saveMetrics = TRUE)

#https://www.rdocumentation.org/packages/caret/versions/6.0-88/topics/nearZeroVar
#freqRatio = ratio of frequencies for most common value over second most common value
#percentunique = percentage of unique data points out of total number of data points


#--------------------------------------
# Modeling
#--------------------------------------
#CREATE TEST AND TRAIN DATA
if (!require(caret)) install.packages('caret')
library(caret)
set.seed(1, sample.kind="Rounding") # if using R 3.5 or earlier, use `set.seed(1)`

test_index <- createDataPartition(y = concrete$compressive_strength, times = 1, p = 0.3, list = FALSE)
train <- concrete[-test_index,]
test <- concrete[test_index,]

#check that the test dataset is representative of the train dataset
summary(train)
summary(test)

#create the RMSE function to measure model performance
RMSE <- function(true_ratings, predicted_ratings){sqrt(mean((true_ratings - predicted_ratings)^2))}

#we will first try to fit a linear model

#linear model - use cement as the only predictor variable
cement_lm = lm(compressive_strength ~ cement, data = train)
summary(cement_lm)
predictions <- predict(cement_lm, newdata = test)
cement_RMSE <- RMSE(test$compressive_strength,predictions)

#create a results table with the RMSE for each model for easy comparison
rmse_results <- tibble(method = "Cement", RMSE = cement_RMSE)
rmse_results


#linear model - use w/c ratio as the only predictor variable to see if this provides improvement over cement as predictor
w_c_ratio_lm <- lm(compressive_strength ~ w_c_ratio, data = train)
summary(w_c_ratio_lm)
predictions <- predict(w_c_ratio_lm, newdata = test)
w_c_ratio_RMSE <- RMSE(test$compressive_strength,predictions)
#rmse table
rmse_results <- rbind(rmse_results,tibble(method = "W/C Ratio", RMSE = w_c_ratio_RMSE))
rmse_results

#linear model - use w/b ratio as the only predictor variable
#since w/b ratio is an extension of the w/c ratio concept, look to see if this improves performance
w_b_ratio_lm <- lm(compressive_strength ~ w_b_ratio, data = train)
summary(w_b_ratio_lm)
predictions <- predict(w_b_ratio_lm, newdata = test)
w_b_ratio_RMSE <- RMSE(test$compressive_strength,predictions)
#rmse table
rmse_results <- rbind(rmse_results,tibble(method = "W/B Ratio", RMSE = w_b_ratio_RMSE))
rmse_results


#linear model - using w/c ratio and w/b ratio 
w_c_b_ratio_lm <- lm(compressive_strength ~ w_c_ratio + w_b_ratio, data = train)
summary(w_c_b_ratio_lm)
predictions <- predict(w_c_b_ratio_lm, newdata = test)
w_c_b_ratio_RMSE <- RMSE(test$compressive_strength,predictions)
#rmse table
rmse_results <- rbind(rmse_results,tibble(method = "W/C and W/B Ratio", RMSE = w_c_b_ratio_RMSE))
rmse_results

#correlation of w/c ratio and w/b ratio
cor(concrete$w_c_ratio, concrete$w_b_ratio)
#these have moderate correlation - makes sense since they are both calculated from water and cement


#linear model - using all of the original variables, without the calculated variables w/c ratio and w/b ratio
simple_lm <-lm(compressive_strength ~ cement + blast_furnace_slag + fly_ash + water + superplasticizer + coarse_aggregate + fine_aggregate + age, data = train)
summary(simple_lm)
predictions <- predict(simple_lm, newdata = test)
simple_RMSE <- RMSE(test$compressive_strength,predictions)
#rmse table
rmse_results <- rbind(rmse_results,tibble(method = "All Features", RMSE = simple_RMSE))
rmse_results

#Variable inflation - https://www.rdocumentation.org/packages/regclass/versions/1.6/topics/VIF
if (!require(regclass)) install.packages('regclass')
library(regclass)
VIF(simple_lm)

#linear model using the engineered features and removing the features that make up the ratios
simple_lm2 <- lm(compressive_strength ~ w_c_ratio + w_b_ratio + superplasticizer + coarse_aggregate + fine_aggregate + age, data = train)
predictions <- predict(simple_lm2, newdata = test)
simple_fit2_RMSE <- RMSE(test$compressive_strength,predictions)  
#rmse table
rmse_results <- rbind(rmse_results,tibble(method = "All Features w/engineering", RMSE = simple_fit2_RMSE))
rmse_results
#variable inflation
VIF(simple_lm2)


#HPC is a highly nonlinear function so not surprised that none of these linear models perform very well
#regularization might improve the performance here by penalizing noisy estimates, but since we know this is a nonlinear problem, will try other models

#try a regression tree
if (!require(rpart)) install.packages('rpart')
library(rpart)
regression_tree_lm <- rpart(compressive_strength~.,data = train)
predictions <- predict(regression_tree_lm, newdata = test)
regression_tree_RMSE <- RMSE(test$compressive_strength,predictions)

#rmse table
rmse_results <- rbind(rmse_results,tibble(method = "Regression Tree", RMSE = regression_tree_RMSE))
rmse_results


#try random forest regression
if (!require(randomForest)) install.packages('randomForest')
library(randomForest)
concrete_rf <-  randomForest(compressive_strength ~ ., data = train, importance = TRUE, na.action = na.omit)
importance(concrete_rf) #print out the importance
predictions <- predict(concrete_rf, newdata = test)
random_forest_RMSE <- RMSE(test$compressive_strength,predictions)
#rmse table
rmse_results <- rbind(rmse_results,tibble(method = "Random Forest", RMSE = random_forest_RMSE))
rmse_results

