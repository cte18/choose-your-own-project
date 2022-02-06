##########################################################
# "Choose Your Own Project"
# for Data Science Capstone Course, February 2022
##########################################################
#
# [Describe the project here]
# 
# Note:
# R Version 4.1.1 was used in this project. Any code required for earlier versions of R has been retained but commented out.

##########################################################
# Setup the environment, load original data and create datasets.
##########################################################

# Note: this process could take a couple of minutes


# Install any packages required.

if(!require(tidyverse))
  install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret))
  install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table))
  install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(scales)) 
  install.packages("scales", repos = "http://cran.us.r-project.org")
if(!require(lubridate)) 
  install.packages("lubridate", repos = "http://cran.us.r-project.org")

library(tidyverse)
library(caret)
library(data.table)
library(scales)
library(lubridate)


##########################################################
# Beginning of code to predict movie ratings in the 
# validation set.
##########################################################


# Examine the sets to understand their structure.


# To see some example data, view the first few rows of the sets.


##########################################################
# Do some simple checks for missing values.
##########################################################

# Check for any NA values in any column in either set.
# apply(edx, 2, function(x) any(is.na(x)))
# apply(validation, 2, function(x) any(is.na(x)))

# Check columns of type "character" for any empty strings.
# edx %>% filter(title =="")
# edx %>% filter(genres =="")
# validation %>% filter(title =="")
# validation %>% filter(genres =="")


##########################################################
# Explore the larger of the two sets - "edx" - 
# to further understand the nature of the data.
##########################################################




##########################################################
# Reformat the data to make it easier to work with.
##########################################################




##########################################################
# Set up the objects required for the modelling.
##########################################################

# Create Training and Test Sets from the "edx" set.

# The following line of code assumes that R 3.6 or later
# is being used.
set.seed(1, sample.kind = "Rounding") 

# Create a training set that is 90% of the edx set,
# and a test set that is 10% of the edx set.
# edx_test_index <- createDataPartition(y = edx$rating, times = 1, p = 0.1, list = FALSE)
# edx_train_set <- edx %>% slice(-edx_test_index)
# temp <- edx %>% slice(edx_test_index)

# For some of the calculations we make later, the users, 
# movies, week_of_ratings, year of release, and genres
# that are in the test set must also be in the training set.
# Therefore, at this point we remove any rows in the test
# set with a user, movie, week_of_rating, year of release,
# or genres that are not in the training set.
# edx_test_set <- temp %>%
#  semi_join(edx_train_set, by = "movieId") %>%
#  semi_join(edx_train_set, by = "userId") %>%


# Add rows removed from the test set back into the
# training set.
# removed <- anti_join(temp, edx_test_set)
# edx_train_set <- rbind(edx_train_set, removed)

# Define a function to calculate the root mean squared
# error (RMSE) of a model.
RMSE <- function(true_ratings, predicted_ratings){
  sqrt(mean((true_ratings - predicted_ratings)^2))
}


##########################################################
# Naive Average Rating Model
# --------------------------
# We start with a very simple model, where every rating is
# predicted to be the same as the average rating of all 
# movies in the training set.
##########################################################




##########################################################
# Predict Ratings in the Validation Set
# ----------------------------------------
# Re-calculates the effects used in the final model based on
# the entire edx set, then uses the final model to produce 
# predicted ratings for the validation set.
##########################################################



