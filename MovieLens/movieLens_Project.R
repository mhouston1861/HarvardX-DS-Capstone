##notes##
#we can assume that files are downloaded to the same working directory

##########################################################
# Create edx set, validation set (final hold-out test set)
##########################################################

# Note: this process could take a couple of minutes

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")

library(tidyverse)
library(caret)
library(data.table)

# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip

dl <- tempfile()
download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

ratings <- fread(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
                 col.names = c("userId", "movieId", "rating", "timestamp"))

movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)
colnames(movies) <- c("movieId", "title", "genres")

# if using R 3.6 or earlier:
movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(levels(movieId))[movieId],
                                           title = as.character(title),
                                           genres = as.character(genres))
# if using R 4.0 or later:
# movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(movieId),
#                                            title = as.character(title),
#                                            genres = as.character(genres))


movielens <- left_join(ratings, movies, by = "movieId")

# Validation set will be 10% of MovieLens data
set.seed(1, sample.kind="Rounding") # if using R 3.5 or earlier, use `set.seed(1)`

test_index <- createDataPartition(y = movielens$rating, times = 1, p = 0.1, list = FALSE)
edx <- movielens[-test_index,]
temp <- movielens[test_index,]

# Make sure userId and movieId in validation set are also in edx set
validation <- temp %>% 
  semi_join(edx, by = "movieId") %>%
  semi_join(edx, by = "userId")

# Add rows removed from validation set back into edx set
removed <- anti_join(temp, validation)
edx <- rbind(edx, removed)

rm(dl, ratings, movies, test_index, temp, movielens, removed)

#save data frames to file to we can load them faster and skip re-running the preceding code
saveRDS(edx,file = "movieLens_edx_dataframe.rds")
saveRDS(validation, "movieLens_validation_dataframe.rds")


##########################################################
#Load Datasets
##########################################################

library(tidyverse)
library(caret)
library(data.table)
library(tidyr)

#open matrix files
edx <- readRDS("movieLens_edx_dataframe.rds")
validation <- readRDS("movieLens_validation_dataframe.rds")

#convert to data frames
edx <- as.data.frame(edx)
validation <- as.data.frame(validation)


##########################################################
#Create test and train dataset
##########################################################

#caret library for model development
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
library(caret)

#split the edx dataset into test and train datasets
test_index <- createDataPartition(y = edx$rating, times = 1, p = 0.2,list = FALSE)
train_set <- edx[-test_index,]
temp_test_set <- edx[test_index,]

#we need to make sure the movies/users are included in both test and train
test_set <- temp_test_set %>% semi_join(train_set, by = 'movieId')%>%
  semi_join(train_set, by = 'userId')

removed <- anti_join(temp_test_set, test_set)
train_set <- rbind(train_set, removed)


##########################################################
#Data Exploration
##########################################################
#see the table in tidy format
edx %>% as_tibble()

#explore structure of data frame
class(edx)
edx_head <- head(edx)
edx_head 
saveRDS(edx_head, file = file.path('exploration','edx_head.rds')) #save for report

#timestamp is not in standard timestamp format
#title includes year of tile
#note the genre column is a pipe-delimited list of genre categories

#get summary data of each column within data frame
edx_summary <- summary(edx)
edx_summary
saveRDS(edx_summary, file = file.path('exploration','edx_summary.rds')) #save for report

#confirm there are no missing values

#number of distinct movies
n_distinct(edx$movieId)

#number of distinct movie ratings
n_distinct(edx$rating)

#distinct genre combinations
n_distinct(edx$genres)

#get the number of distinct users and movies
edx_distinct_um <- edx%>%summarize(users = n_distinct(userId),movies = n_distinct(movieId))
edx_distinct_um
saveRDS(edx_distinct_um, file = file.path('exploration','edx_distinct_um.rds')) #save to file for report

# separate_rows -> noted that this is a slow approach but see distribution by genre
genres <- edx %>% separate_rows(genres, sep = "\\|") %>%
  group_by(genres) %>%
  summarize(count = n()) %>%
  arrange(desc(count))
saveRDS(genres, file = "distinct_genres.rds") #save to file for report

#look at the number of movies by movieId (i.e. number of ratings)
edx %>% count(movieId) %>% 
  ggplot(aes(n)) + geom_histogram(bins = 30, color = "black") +scale_x_log10() + xlab("Number of Ratings") + ylab("Movies")
img_file <- file.path('images','moviecount.png')
ggsave(img_file)

#look at distribution of how activer users are (entries per user Id)
edx %>% count(userId) %>% 
  ggplot(aes(n)) + geom_histogram(bins = 30, color = "black")+scale_x_log10() + xlab("Number of Ratings") + ylab("Users")
img_file <- file.path('images','usercount.png')
ggsave(img_file)


#distribution of ratings
edx %>%ggplot(aes(x=rating))+ geom_bar()
img_file <- file.path('images','ratingscount.png')
ggsave(img_file)

edx %>% group_by(rating)%>%summarize(count = n()) %>% arrange(count) #see the number of each rating

#find the lowest rated movies
edx%>% filter(rating == 0.5)%>%group_by(movieId, title)%>%count(movieId)%>%arrange(desc(n))

lowest_rated <- edx %>% group_by(movieId, title)%>%
  summarize(count = n(), avg_rating = sum(rating)/(n())) %>%
  arrange(avg_rating) %>% head()
lowest_rated
saveRDS(lowest_rated, file = file.path('exploration','lowest_rated.rds')) #save for report

#highest rated movies
highest_rated <- edx %>% group_by(movieId, title)%>%summarize(count = n(), avg_rating = sum(rating)/(n())) %>% arrange(desc(count), desc(avg_rating))%>%head()
highest_rated
saveRDS(highest_rated, file = file.path('exploration','highest_rated.rds')) #save for report

#highest rated movies
edx%>% filter(rating == 5)%>%group_by(movieId, title)%>%count(movieId)%>%arrange(desc(n))

#see 20 most active users
edx %>% count(userId)%>%arrange(desc(n))%>% head(20)

#average ratings across users
edx %>% group_by(userId)%>%summarize(avgRating = mean(rating))%>%
  ggplot(aes(avgRating)) + geom_histogram(bins = 10, color = "black") + xlab("Average Rating") + ylab("Number of users")
img_file <- file.path('images','useravgratings.png')
ggsave(img_file)

##########################################################
#Simple model - average rating
##########################################################

#create the RMSE function
RMSE <- function(true_ratings, predicted_ratings){sqrt(mean((true_ratings - predicted_ratings)^2))}


#the simplest model we start with is predicting the same rating for all movies regardless of user
#we minimize the RMSE with the least squares estimate of mu which is the average of all ratings
mu_hat <- mean(train_set$rating)
mu_hat
saveRDS(mu_hat, file = file.path('model_outputs','mu_hat.rds'))

#predict all unknown ratings with mu_hat - obtain the RMSE
naive_rmse <- RMSE(test_set$rating, mu_hat)
naive_rmse

#any other number will give us a higher RMSE
predictions <- rep(2.5, nrow(test_set))
RMSE(test_set$rating, predictions)

#create a results table with the RMSE for each model
rmse_results <- tibble(model_number = "1", method = "Just the average", RMSE = naive_rmse)
saveRDS(rmse_results,file = file.path('model_outputs','model_1_rmse_results.rds')) #save to file for report
rmse_results


##########################################################
#modeling movie effects
##########################################################
#we can augment our model by adding the term b_i to represent average ranking for movie i:
#Y_u_i = mu + b_i + e_u_i

#instead of using lm know that the least squares estimate is just the average of Y_u_i-mu_hat for each i
mu <- mean(train_set$rating)
movie_avgs <- train_set %>% 
  group_by(movieId) %>% #group by movie Id
  summarize(b_i = mean(rating - mu)) #calculate Y_u_i - mu_hat for each i

#plot these estimates
qplot(b_i,data=movie_avgs,bins=10, color=I("black"))

#see how much prediction improves by using the b_i
predicted_ratings <-  test_set %>%
  left_join(movie_avgs,by='movieId')%>%
  mutate(pred = mu + b_i)%>%
  pull(pred)
movie_rmse <- RMSE(predicted_ratings, test_set$rating)
rmse_results <- rbind(rmse_results,tibble(model_number = "2", method = "With movie effect", RMSE = movie_rmse))
saveRDS(rmse_results,file = file.path('model_outputs','model_2_rmse_results.rds')) #save to file for report
rmse_results


##########################################################
#USER EFFECTS#
##########################################################
#compute the average rating for user u for those who have rated 100 or more movies
train_set %>% 
  group_by(userId)%>% #group by each user
  filter(n()>=100)%>% #filter to only those with 100 or more ratings
  summarize(b_u=mean(rating))%>% #b_u is the average rating per user
  ggplot(aes(b_u))+geom_histogram(bins = 30,color="black")

#we see variablity across users, which implies some users generally give low ratings and some users generally give high ratings
#we compute an approximation by computing mu_hat and b_i_hat and estimate b_u_hat as the average of y_u_i - mu_hat - b_i_hat
user_avgs <- train_set %>% 
  left_join(movie_avgs,by='movieId')%>%
  group_by(userId)%>%
  summarize(b_u = mean(rating-mu-b_i))
#predict
predicted_ratings <- test_set %>% 
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  mutate(pred = mu + b_i + b_u) %>%
  pull(pred)
#see if the RMSE improves
user_rmse <- RMSE(predicted_ratings, test_set$rating)
rmse_results <- rbind(rmse_results,tibble(model_number = "3", method = "With movie + user effect", RMSE = user_rmse))
saveRDS(rmse_results,file = file.path('model_outputs','model_3_rmse_results.rds')) #save to file for report
rmse_results




##########################################################
#genre effect
##########################################################
#naive genre effect - we consider each unique genre category (pipe delimited list of genres)
#we can add a genre term b_g in the same manner
genre_cat_avgs <- train_set %>% 
  left_join(movie_avgs,by='movieId')%>%
  left_join(user_avgs, by = 'userId')%>%
  group_by(genres)%>%
  summarize(b_g = mean(rating-mu-b_i-b_u))
#predict
predicted_ratings <- test_set %>% 
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  left_join(genre_cat_avgs, by='genres')%>%
  mutate(pred = mu + b_i + b_u + b_g) %>%
  pull(pred)
#see if the RMSE improves
genre_rmse <- RMSE(predicted_ratings, test_set$rating)
rmse_results <- rbind(rmse_results,tibble(model_number = "4", method = "With movie + user +genre effect", RMSE = genre_rmse))
saveRDS(rmse_results,file = file.path('model_outputs','model_4_rmse_results.rds')) #save to file for report
rmse_results

#we see no substantial RMSE Improvement which makes intuitive sense  
#we would expect the genre effect to be based on individual genres, not the categories (i.e. 'Drama' not 'Action | Drama | Romance')
#we would expect to model this as
#from the machine learning course
#if we define g_u_i as the genre for user u's rating of movie i, genre effect can be described as:
#Y_u_i = mu + b_i + b_u + SUM_k=1_K(x_u_i^k*B_k) + e_u_i  with x_u_i^k = 1 if g_u_i is genre k


##########################################################
#REGULARIZATION#
##########################################################

#look at largest errors in predictions

####we can look at the movie effect - largest residual differences
top_10_errors <- test_set %>% 
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by = 'userId')%>%
  left_join(genre_cat_avgs, by = 'genres')%>%
  mutate(residual = rating - (mu + b_i+b_u+b_g)) %>%
  arrange(desc(abs(residual))) %>% head(10) 
saveRDS(top_10_errors,file = file.path('model_outputs','top_10_errors.rds')) #save to file for report

top_10_errors%>%select(movieId, title,rating,residual)%>%arrange(movieId)

#see how many times they have been rated in the dataset
top_10_errors_ratingcount <- top_10_errors%>%group_by(movieId, title) %>% 
  left_join(train_set, by = 'movieId')%>%
  summarise(count = n())%>%arrange(movieId)
top_10_errors_ratingcount
saveRDS(top_10_errors_ratingcount, file = file.path('model_outputs','top_10_errors_ratingcount.rds'))
#we can penalize movies with very few ratings as these have much more uncertainty
#these are noisy estimates that we should not trust 
#we optimize to be bigger when the estimates b are far from zero

#use cross-validation to choose lambda
#to do this we need to to separate train into train and validation sets (k-fold cross validation means to do this split k times)
#if k = 5 then we split training 5 different times to create 5 sets of train and validation

k <-5 #we will do 5-fold cross validation

#we will use the caret package to create our k-folds
#note that we create the k-folds from our training set - we leave the test set for verification
folds <- createFolds(train_set$rating, k, list = TRUE, returnTrain = FALSE)


#define lambdas to test
lambdas <- seq(0,10,0.25)
ncols <- length(lambdas)

#create matrix to store the results of cross validation
rmses <- matrix(nrow = k,ncol = ncols)


#for each fold in k-folds
for(k in 1:5){
  #create train and test set for each fold
  k_train_set <- train_set[folds[[k]],]
  k_test_set <- train_set[-folds[[k]],]
  
  #make sure userId and movieId in test set are also in the train set
  k_test_final <- k_test_set %>% 
    semi_join(k_train_set, by = "movieId") %>%
    semi_join(k_train_set, by = "userId")
  
  #add rows removed from test set back into train set
  k_removed <- anti_join(k_test_set, k_test_final)
  k_train_final <- rbind(k_train_set, k_removed)
  
  mu <- mean(k_train_final$rating) #movie effect
  just_the_sum <- k_train_final %>% group_by(movieId)%>%
    summarize(s = sum(rating - mu),n_i=n())
  
  rmses[k,] <- sapply(lambdas, function(l){
    predicted_ratings <- k_test_final %>% 
      left_join(just_the_sum, by='movieId') %>% 
      mutate(b_i = s/(n_i+l)) %>%
      mutate(pred = mu + b_i) %>%
      pull(pred)
    return(RMSE(predicted_ratings, k_test_final$rating))
  })
}

#for each lambda, take the mean of the RMSES
rmses_cv <- colMeans(rmses)
saveRDS(rmses_cv, file = file.path('model_outputs','movie_reg_rmses_cv.rds')) #save to file for report
qplot(lambdas,rmses_cv) #plot lambdas vs rmses to see the lambda which minimizes 
#find the lambda with the minimum average RMSEs
lambda <- lambdas[which.min(rmses_cv)]
lambda

#apply the lambda to the model
mu <- mean(train_set$rating)
movie_reg_avgs <- train_set %>% 
  group_by(movieId) %>% 
  summarize(b_i = sum(rating - mu)/(n()+lambda), n_i = n()) 

#plot original estimates vs regularized estimates
tibble(original = movie_avgs$b_i, 
       regularized = movie_reg_avgs$b_i, 
       n = movie_reg_avgs$n_i) %>%
  ggplot(aes(original, regularized, size=sqrt(n))) + 
  geom_point(shape=1, alpha=0.5)
img_file <- file.path('images','movie_reg_bi.png')
ggsave(img_file)

#see if RMSE improved
predicted_ratings <- test_set %>% 
  left_join(movie_reg_avgs, by = "movieId") %>%
  mutate(pred = mu + b_i) %>%
  pull(pred)
#saveRDS(predicted_ratings, file = file.path('model_outputs','movie_reg_predictions.rds'))
movie_reg_RMSE <- RMSE(predicted_ratings, test_set$rating)
rmse_results <- rbind(rmse_results,tibble(model_number = "5", method = "Regularized movie effect", RMSE = movie_reg_RMSE))
saveRDS(rmse_results,file = file.path('model_outputs','model_5_rmse_results.rds')) #save to file for report
rmse_results

##########################################################
#regularize with model and user
##########################################################
k <-5 #we will do 5-fold cross validation

#we will use the caret package to create our k-folds
folds <- createFolds(train_set$rating, k, list = TRUE, returnTrain = FALSE)

#define lambdas to test
lambdas <- seq(0,10,0.25)
ncols <- length(lambdas)

#create matrix to store the results of cross validation
rmses <- matrix(nrow = k,ncol = ncols)

#for each fold in k-folds
for(k in 1:5){
  #create train and test set for each fold
  k_train_set <- train_set[folds[[k]],]
  k_test_set <- train_set[-folds[[k]],]
  
  #make sure userId and movieId in test set are also in the train set
  k_test_final <- k_test_set %>% 
    semi_join(k_train_set, by = "movieId") %>%
    semi_join(k_train_set, by = "userId")
  
  #add rows removed from test set back into train set
  k_removed <- anti_join(k_test_set, k_test_final)
  k_train_final <- rbind(k_train_set, k_removed)
  
  rmses[k,] <- sapply(lambdas, function(l){
    
    mu <- mean(k_train_final$rating) #mean rating
    
    b_i <- k_train_final %>% group_by(movieId)%>%
      summarize(b_i = sum(rating - mu)/(n()+l)) #movie effect
    
    b_u <- k_train_final %>% 
      left_join(b_i,by='movieId')%>%
      group_by(userId)%>%
      summarize(b_u = sum(rating-mu-b_i)/(n() + l))
    
    predicted_ratings <- k_test_final %>% 
      left_join(b_i, by='movieId') %>% 
      left_join(b_u, by = 'userId')%>%
      mutate(pred = mu + b_i + b_u) %>%
      pull(pred)
    return(RMSE(predicted_ratings, k_test_final$rating))
  })
}


#for each lambda, take the mean of the RMSES
rmses_cv <- colMeans(rmses)
saveRDS(rmses_cv, file.path('model_outputs', "movieuser_reg_rmses_cv.rds")) #save to file for report
qplot(lambdas,rmses_cv)
#find the lambda with the minimum average RMSEs
lambda <- lambdas[which.min(rmses_cv)]

#apply the lambda to the model
mu <- mean(train_set$rating)

movie_reg_avgs <- train_set %>% 
  group_by(movieId) %>% 
  summarize(b_i = sum(rating - mu)/(n()+lambda)) 

user_reg_avgs <- train_set %>%
  left_join(movie_reg_avgs,by='movieId')%>%
  group_by(userId)%>%
  summarize(b_u = sum(rating-mu-b_i)/(n() + lambda), n_i = n())

#plot original estimates against regularized
tibble(original = user_avgs$b_u, 
       regularized = user_reg_avgs$b_u, 
       n = user_reg_avgs$n_i) %>%
  ggplot(aes(original, regularized, size=sqrt(n))) + 
  geom_point(shape=1, alpha=0.5)
img_file <- file.path('images','movieuser_reg_bu.png')
ggsave(img_file)

#see if RMSE improved
predicted_ratings <- test_set %>% 
  left_join(movie_reg_avgs, by = "movieId") %>%
  left_join(user_reg_avgs, by = 'userId')%>%
  mutate(pred = mu + b_i + b_u) %>%
  pull(pred)
user_reg_RMSE <- RMSE(predicted_ratings, test_set$rating)
rmse_results <- rbind(rmse_results,tibble(model_number = "6", method = "Regularized movie + user effect", RMSE = user_reg_RMSE))
saveRDS(rmse_results,file = file.path('model_outputs','model_6_rmse_results.rds')) #save to file for report
rmse_results


##########################################################
###regularize with model + user + genre effect
##########################################################
k <-5 #we will do 5-fold cross validation

#we will use the caret package to create our k-folds
folds <- createFolds(train_set$rating, k, list = TRUE, returnTrain = FALSE)

#define lambdas to test
lambdas <- seq(0,10,0.25)
ncols <- length(lambdas)

#create matrix to store the results of cross validation
rmses <- matrix(nrow = k,ncol = ncols)

#for each fold in k-folds
for(k in 1:5){
  #create train and test set for each fold
  k_train_set <- train_set[folds[[k]],]
  k_test_set <- train_set[-folds[[k]],]
  
  #make sure userId and movieId in test set are also in the train set
  k_test_final <- k_test_set %>% 
    semi_join(k_train_set, by = "movieId") %>%
    semi_join(k_train_set, by = "userId")
  
  #add rows removed from test set back into train set
  k_removed <- anti_join(k_test_set, k_test_final)
  k_train_final <- rbind(k_train_set, k_removed)
  
  rmses[k,] <- sapply(lambdas, function(l){
    
    mu <- mean(k_train_final$rating) #mean rating
    
    b_i <- k_train_final %>% group_by(movieId)%>%
      summarize(b_i = sum(rating - mu)/(n()+l)) #movie effect
    
    b_u <- k_train_final %>% 
      left_join(b_i,by='movieId')%>%
      group_by(userId)%>%
      summarize(b_u = sum(rating-mu-b_i)/(n() + l))
    
    b_g <-  k_train_final %>% 
      left_join(b_i,by='movieId')%>%
      left_join(b_u, by = 'userId')%>%
      group_by(genres)%>%
      summarize(b_g = sum(rating-mu-b_i-b_u)/(n() + l))
    
    predicted_ratings <- k_test_final %>% 
      left_join(b_i, by='movieId') %>% 
      left_join(b_u, by = 'userId')%>%
      left_join(b_g, by = 'genres')%>%
      mutate(pred = mu + b_i + b_u + b_g) %>%
      pull(pred)
    return(RMSE(predicted_ratings, k_test_final$rating))
  })
}


#for each lambda, take the mean of the RMSES
rmses_cv <- colMeans(rmses)
saveRDS(rmses_cv, file.path('model_outputs', "movieusergenres_reg_rmses_cv.rds")) #save to file for report
qplot(lambdas,rmses_cv)
#find the lambda with the minimum average RMSEs
lambda <- lambdas[which.min(rmses_cv)]

#apply the lambda to the model
mu <- mean(train_set$rating)

movie_reg_avgs <- train_set %>% 
  group_by(movieId) %>% 
  summarize(b_i = sum(rating - mu)/(n()+lambda)) 

user_reg_avgs <- train_set %>%
  left_join(movie_reg_avgs,by='movieId')%>%
  group_by(userId)%>%
  summarize(b_u = sum(rating-mu-b_i)/(n() + lambda))

genre_reg_avgs <- train_set %>%
  left_join(movie_reg_avgs,by='movieId')%>%
  left_join(user_reg_avgs,by='userId')%>%
  group_by(genres)%>%
  summarize(b_g = sum(rating-mu-b_i-b_u)/(n() + lambda), n_i = n())

#plot original estimates against regularized
tibble(original = genre_cat_avgs$b_g, 
       regularized = genre_reg_avgs$b_g, 
       n = genre_reg_avgs$n_i) %>%
  ggplot(aes(original, regularized, size=sqrt(n))) + 
  geom_point(shape=1, alpha=0.5)
img_file <- file.path('images','movieusergenres_reg_bg.png')
ggsave(img_file)

#see if RMSE improved
predicted_ratings <- test_set %>% 
  left_join(movie_reg_avgs, by = "movieId") %>%
  left_join(user_reg_avgs, by = 'userId')%>%
  left_join(genre_reg_avgs,by='genres')%>%
  mutate(pred = mu + b_i + b_u) %>%
  pull(pred)
genre_reg_RMSE <- RMSE(predicted_ratings, test_set$rating)
rmse_results <- rbind(rmse_results,tibble(model_number = "7", method = "Regularized movie + user + genre effect", RMSE = genre_reg_RMSE))
saveRDS(rmse_results,file = file.path('model_outputs','model_7_rmse_results.rds')) #save to file for report
rmse_results 

##########################################################
### Matrix Factorization
##########################################################

if(!require(recosystem)) install.packages("recosystem", repos = "http://cran.us.r-project.org")
library(recosystem)
r <- Reco()

#put our training data into the correct format for the recosystem package
train_data <- with(train_set, data_memory(user_index = userId, item_index = movieId, rating = rating))
test_data <- with(test_set, data_memory(user_index = userId, item_index = movieId, rating = rating))

#here we use the standard defaults for the package
opts_tune = r$tune(train_data,                                   # 9
                   opts = list(dim      = c(10, 20, 30),        # 10
                               costp_l2 = c(0.01, 0.1),         # 11
                               costq_l2 = c(0.01, 0.1),         # 12
                               costp_l1 = 0,                    # 13
                               costq_l1 = 0,                    # 14
                               lrate    = c(0.01, 0.1),         # 15
                               nthread  = 4,                    # 16
                               niter    = 10,                   # 17
                               verbose  = FALSE))                # 18
r$train(train_data, opts = c(opts_tune$min,                      # 19
                             niter = 20, nthread = 4)) 

#predict ratings on the test data
ratings_predicted <- r$predict(test_data, out_memory())

#calculate the RMSE
matrix_fact_RMSE <- RMSE(test_set$rating, ratings_predicted)
rmse_results <- rbind(rmse_results,tibble(model_number = "8", method = "Matrix Factorization", RMSE = matrix_fact_RMSE))
saveRDS(rmse_results,file = file.path('model_outputs','model_8_rmse_results.rds')) #save to file for report
rmse_results

##########################################################
### Final Model - Matrix Factorization
##########################################################

#retrain the model on the edx dataset
edx_train_data <- with(edx, data_memory(user_index = userId, item_index = movieId, rating = rating))
r$train(edx_train_data, opts = c(opts_tune$min,                      # 19
                             niter = 100, nthread = 4)) 

#test on the final validation dataset to obtain the final RMSE
validation_data <- with(validation, data_memory(user_index = userId, item_index = movieId, rating = rating))
final_predictions <- r$predict(validation_data, out_memory())

#calculate the final RMSE
final_RMSE <- RMSE(final_predictions, validation$rating)
saveRDS(final_RMSE,file = file.path('model_outputs','final_rmse.rds')) #save to file for report
final_RMSE