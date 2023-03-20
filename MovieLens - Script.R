##########################################################
# Create edx and final_holdout_test sets 
##########################################################

# Note: this process could take a couple of minutes

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(stringr)) install.packages("stringr", repos = "https://stringr.tidyverse.org/")
if(!require(readr)) install.packages("readr", repos = "https://readr.tidyverse.org/")
if(!require(dplyr)) install.packages("dplyr", repos = "https://dbplyr.tidyverse.org/")



# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip

options(timeout = 120)

dl <- "ml-10M100K.zip"
if(!file.exists(dl))
  download.file("https://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

ratings_file <- "ml-10M100K/ratings.dat"
if(!file.exists(ratings_file))
  unzip(dl, ratings_file)

movies_file <- "ml-10M100K/movies.dat"
if(!file.exists(movies_file))
  unzip(dl, movies_file)

ratings <- as.data.frame(str_split(read_lines(ratings_file), fixed("::"), simplify = TRUE),
                         stringsAsFactors = FALSE)
colnames(ratings) <- c("userId", "movieId", "rating", "timestamp")
ratings <- ratings %>%
  mutate(userId = as.integer(userId),
         movieId = as.integer(movieId),
         rating = as.numeric(rating),
         timestamp = as.integer(timestamp))

movies <- as.data.frame(str_split(read_lines(movies_file), fixed("::"), simplify = TRUE),
                        stringsAsFactors = FALSE)
colnames(movies) <- c("movieId", "title", "genres")
movies <- movies %>%
  mutate(movieId = as.integer(movieId))

movielens <- left_join(ratings, movies, by = "movieId")

# Final hold-out test set will be 10% of MovieLens data
set.seed(1, sample.kind="Rounding") # if using R 3.6 or later
set.seed(1) # if using R 3.5 or earlier

test_index <- createDataPartition(y = movielens$rating, times = 1, p = 0.1, list = FALSE)
edx <- movielens[-test_index,]
temp <- movielens[test_index,]

# Make sure userId and movieId in final hold-out test set are also in edx set

final_holdout_test <- temp %>% 
  semi_join(edx, by = "movieId") %>%
  semi_join(edx, by = "userId")

# Add rows removed from final hold-out test set back into edx set

removed <- anti_join(temp, final_holdout_test)
edx <- rbind(edx, removed)

rm(dl, ratings, movies, test_index, temp, movielens, removed, movies_file, ratings_file) #movies_file and ratings_file add on this list by me

########################################################

## First lets making some changes at the data.table for better visualization
if(!require(lubridate)) install.packages("lubridate", repos = "https://lubridate.tidyverse.org/") #package to work with dates

#transforming timestamp in year_rated
edx <- edx %>% mutate(year_rated = year(as_datetime(timestamp))) %>% select(-timestamp)

#creating a new column presenting the year_release
edx <- edx %>% mutate(year_release = as.numeric(str_sub(edx$title, start = -5, end = -2)))



## Calculating the RMSE
#Our goal here is to get the minimum value of RMSE possible for a better result.
# RMSE means "Root of mean of square errors", and to calculate it we are going to create a function: 
RMSE <- function(rating_from_test_set, predicted_ratings){
  sqrt(mean((rating_from_test_set - predicted_ratings)^2))
}


## 
# To calculate the first model of rating we are use de mean of the total rating of all the movies
meanRating <- edx %>% group_by(movieId) %>% filter(n() > 100) %>% ungroup()
meanRating <- mean(meanRating$rating)
meanRating # the mean is 3.520019

# If we only use this mean to predict the rating we will reach a RMSE 1.061232
AverageRatingRMSE <- RMSE(final_holdout_test$rating, meanRating)
AverageRatingRMSE


## 
# Now we are going to improve the results with the user effect

# Lets notice that each movie has its unique effect on people and predict the rating using the estimated deviation of each one of these movies.

userMean <- edx %>% group_by(userId) %>% summarize(estDeviUser = mean(rating - meanRating))

predUserRating <- final_holdout_test %>%
  left_join(userMean, by = 'userId') %>%
  mutate(recommendation = estDeviUser + meanRating) %>%
  select(userId, recommendation)


RMSEpredUserRating <- RMSE(final_holdout_test$rating, predUserRating$recommendation)
RMSEpredUserRating # now we can see that the RMSE is lower than before.It reached 0.978336



##
#The same way that movie has its singularity, people also does have.
#First we compute the estimated deviation of the user

x <- predUserRating %>% group_by(userId) %>% mutate(rec = mean(recommendation)) %>% select(-recommendation)
movieMean <- edx %>% 
  left_join(userMean, by ='userId') %>%
  group_by(movieId) %>%
  summarize(estDeviMovie = mean(rating - meanRating - estDeviUser))

predMovieRating <- final_holdout_test %>%
  left_join(userMean, by='userId') %>%
  left_join(movieMean, by='movieId') %>%
  mutate( pred = meanRating + estDeviMovie + estDeviUser) %>%
  pull(pred)

predMovieRating <- RMSE(predMovieRating, final_holdout_test$rating)

predMovieRating # now the RMSE reached 0.8816096


