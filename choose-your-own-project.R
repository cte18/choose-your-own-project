#########################################################################################
# "Choose Your Own Project - Predicting Heart Disease"
# for Data Science Capstone Course, 8 March 2022
#########################################################################################
#
# The aim of this project was to explore a number of different models that might be
# used to predict the presence of heart disease, and identify the most accurate model
# based on the Cleveland heart disease dataset.
# 
# Note: # R Version 4.1.1 was used in this project.
#
#########################################################################################


#########################################################################################
# Install any packages required.
#########################################################################################

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

#########################################################################################
# 
# Load the dataset.
#
# The original dataset is available in the UCI Machine Learning Repository, at:
# https://archive.ics.uci.edu/ml/machine-learning-databases/heart-disease
# 
# This project uses a copy of the dataset, which was uploaded to a GitHub repository,
# along with the project report and the associated scripts.
# 
#########################################################################################

# Read in the data from the "processed.cleveland.data" file.
cleveland_heart_disease <- read.csv("processed.cleveland.data", 
                                    header = FALSE, sep = ",", stringsAsFactors = FALSE)

# The data file does not contain a header row, so the attribute/column names need to be
# set, using the information provided with the dataset in the "heart-disease.names" file.

column_names <- c("age", "sex",	"cp",	"trestbps",	"chol",	"fbs", "restecg",
                  "thalach", "exang", "oldpeak", "slope", "ca", "thal", "target")
colnames(cleveland_heart_disease) <- column_names


#########################################################################################
# Examine the dataset.
#########################################################################################

# Examine the structure of the dataset.
str(cleveland_heart_disease)

# To see some example data, view the first few rows of the sets.
knitr::kable(head(cleveland_heart_disease, 10))

# Check for any missing ("NA") values in any column.
apply(cleveland_heart_disease, 2, function(x) any(is.na(x)))

# Check for any values containing a "?".
apply(cleveland_heart_disease, 2, function(x) any(x=="?"))


#########################################################################################
# Reformat the data to make it easier to work with.
#########################################################################################

# Remove the rows with "?" values.
cleveland_heart_disease <- cleveland_heart_disease %>% filter(ca!="?" & thal !="?")

# Because the "ca" and "thal" columns contained "?", they were both loaded as type
# "character" initially. They can now be converted to numerics.
cleveland_heart_disease$ca <- as.numeric(cleveland_heart_disease$ca)
cleveland_heart_disease$thal <- as.numeric(cleveland_heart_disease$thal)

# Convert the "target" attribute to a binary value where:
#    0 indicates no heart disease is present
#    1 indicates some heart disease is present
#
# Note: This is the attribute that the models will be trained to predict.

cleveland_heart_disease <- cleveland_heart_disease %>% 
  mutate(target = ifelse(target >= 1, 1, 0))

# Several of the attributes in the dataset are categorical. 
# Convert the categorical attributes from numeric to factor.
categorical_column_names <- c("sex", "cp", "fbs", "restecg", "exang", "slope", "thal",
                              "target")
cleveland_heart_disease[categorical_column_names] <- 
  lapply(cleveland_heart_disease[categorical_column_names], factor)

# Define level names for the "sex" attribute, to make plotting easier.
levels(cleveland_heart_disease$sex)[levels(cleveland_heart_disease$sex)==0] <- 
  "Female"
levels(cleveland_heart_disease$sex)[levels(cleveland_heart_disease$sex)==1] <- 
  "Male"

# Define level names for the "cp" attribute, to make plotting easier.
levels(cleveland_heart_disease$cp)[levels(cleveland_heart_disease$cp)==1] <- 
  "Typical Angina"
levels(cleveland_heart_disease$cp)[levels(cleveland_heart_disease$cp)==2] <- 
  "Atypical Angina"
levels(cleveland_heart_disease$cp)[levels(cleveland_heart_disease$cp)==3] <- 
  "Non-Anginal Pain"
levels(cleveland_heart_disease$cp)[levels(cleveland_heart_disease$cp)==4] <- 
  "Asymptomatic"

# Define level names for the "fbs" attribute, to make plotting easier.
levels(cleveland_heart_disease$fbs)[levels(cleveland_heart_disease$fbs)==0] <- 
  "<= 120 mg/dl"
levels(cleveland_heart_disease$fbs)[levels(cleveland_heart_disease$fbs)==1] <- 
  "> 120 mg/dl"

# Define level names for the "restecg" attribute, to make plotting easier.
levels(cleveland_heart_disease$restecg)[levels(cleveland_heart_disease$restecg)==0] <- 
  "Normal"
levels(cleveland_heart_disease$restecg)[levels(cleveland_heart_disease$restecg)==1] <- 
  "ST-T Wave Abnormality"
levels(cleveland_heart_disease$restecg)[levels(cleveland_heart_disease$restecg)==2] <- 
  "Left Ventricular Hypertrophy"

# Define level names for the "exang" attribute, to make plotting easier.
levels(cleveland_heart_disease$exang)[levels(cleveland_heart_disease$exang)==0] <- 
  "No"
levels(cleveland_heart_disease$exang)[levels(cleveland_heart_disease$exang)==1] <- 
  "Yes"

# Define level names for the "slope" attribute, to make plotting easier.
levels(cleveland_heart_disease$slope)[levels(cleveland_heart_disease$slope)==1] <- 
  "Upsloping"
levels(cleveland_heart_disease$slope)[levels(cleveland_heart_disease$slope)==2] <- 
  "Flat"
levels(cleveland_heart_disease$slope)[levels(cleveland_heart_disease$slope)==3] <- 
  "Downsloping"

# Define level names for the "thal" attribute, to make plotting easier.
levels(cleveland_heart_disease$thal)[levels(cleveland_heart_disease$thal)==3] <- 
  "Normal"
levels(cleveland_heart_disease$thal)[levels(cleveland_heart_disease$thal)==6] <- 
  "Fixed Defect"
levels(cleveland_heart_disease$thal)[levels(cleveland_heart_disease$thal)==7] <- 
  "Reversible Defect"

# Define level names for the "target" attribute, to make plotting easier.
levels(cleveland_heart_disease$target)[levels(cleveland_heart_disease$target)==0] <- 
  "No Heart Disease"
levels(cleveland_heart_disease$target)[levels(cleveland_heart_disease$target)==1] <- 
  "Heart Disease"


# Examine and confirm the structure of the reformatted dataset.
str(cleveland_heart_disease)


#########################################################################################
# Explore the dataset, to further understand the nature of the data.
#########################################################################################

# Display the total number of patients with and without heart disease.
count_by_target <- cleveland_heart_disease %>% 
  count(target) %>% 
  rename("Presence of Heart Disease" = target, "Count" = n)
knitr::kable(count_by_target, 
             caption = "Number of Patients with and without Heart Disease")

# Display graphs for each attribute, as follows.
#
# For each numerical attribute, display a histogram that shows the distribution of 
# patients with and without heart disease (in red and green respectively), overlaid on
# each other.
#
# For each categorical attribute:
# 1. Display a bar graph that shows the proportion of patients with and without heart
#    disease (in red and green respectively), grouped by the value of the categorical
#    attribute.
# 2. Count the number of patients for each value of the categorical attribute.


################################
# Age
################################

cleveland_heart_disease %>%
  ggplot(aes(age, fill = target)) +
  geom_density(alpha = 0.4) +
  ggtitle(label = "Age (in years)", subtitle = " ") +
  xlab(NULL) +
  ylab("Density") + 
  scale_fill_manual(name="Legend", values = c("darkseagreen3", "indianred2")) + 
  theme(legend.position = "right")


################################
# Sex
################################

cleveland_heart_disease %>% 
  group_by(sex,target) %>% 
  summarise(count=n()) %>% 
  mutate(percentage = count/sum(count)) %>%
  ggplot(aes(sex, y = percentage, fill = target)) +
  geom_bar(stat = "identity") + 
  ggtitle(label = "Sex (Female or Male)", subtitle = " ") +
  xlab(NULL) +
  scale_y_continuous(name = "Percentage with / without Heart Disease", 
                     labels = label_percent()) +
  scale_fill_manual(name = "Legend", values = c("darkseagreen3", "indianred2")) +
  theme(legend.position = "right")

count_by_sex <- cleveland_heart_disease %>% 
  count(sex) %>% 
  rename("Sex" = sex, "Count" = n)
knitr::kable(count_by_sex, 
             caption = "Number of patients grouped by sex (female or male).")


################################
# Type of Chest pain (cp)
################################

cleveland_heart_disease %>% 
  group_by(cp,target) %>% 
  summarise(count=n()) %>% 
  mutate(percentage = count/sum(count)) %>%
  ggplot(aes(cp, y = percentage, fill = target)) +
  geom_bar(stat = "identity") + 
  ggtitle(label = "Type of Chest Pain", subtitle = " ") +
  xlab(NULL) +
  scale_y_continuous(name = "Percentage with / without Heart Disease", 
                     labels = label_percent()) +
  scale_fill_manual(name = "Legend", values = c("darkseagreen3", "indianred2")) +
  theme(legend.position = "right")

count_by_cp <- cleveland_heart_disease %>% 
  count(cp) %>% 
  rename("Chest Pain" = cp, "Count" = n)
knitr::kable(count_by_cp, 
             caption = "Number of patients grouped by chest pain category.")


################################
# Resting blood pressure (trestbps)
################################

cleveland_heart_disease %>%
  ggplot(aes(trestbps, fill = target)) +
  geom_density(alpha = 0.4) +
  ggtitle(label = "Resting Blood Pressure (in mm Hg)", subtitle = " ") +
  xlab(NULL) +
  ylab("Density") + 
  scale_fill_manual(name="Legend", values = c("darkseagreen3", "indianred2")) + 
  theme(legend.position = "right")


################################
# Serum cholesterol (chol)
################################

cleveland_heart_disease %>%
  ggplot(aes(chol, fill = target)) +
  geom_density(alpha = 0.4) +
  ggtitle(label = "Serum Cholesterol (in mg/dl)", subtitle = " ") +
  xlab(NULL) +
  ylab("Density") + 
  scale_fill_manual(name="Legend", values = c("darkseagreen3", "indianred2")) + 
  theme(legend.position = "right")


################################
# Fasting blood sugar (fbs)
################################

cleveland_heart_disease %>% 
  group_by(fbs,target) %>% 
  summarise(count=n()) %>% 
  mutate(percentage = count/sum(count)) %>%
  ggplot(aes(fbs, y = percentage, fill = target)) +
  geom_bar(stat = "identity") + 
  ggtitle(label = "Fasting Blood Sugar", subtitle = " ") +
  xlab(NULL) +
  scale_y_continuous(name = "Percentage with / without Heart Disease", 
                     labels = label_percent()) +
  scale_fill_manual(name = "Legend", values = c("darkseagreen3", "indianred2")) +
  theme(legend.position = "right")

count_by_fbs <- cleveland_heart_disease %>% 
  count(fbs) %>% 
  rename("Fasting Blood Sugar" = fbs, "Count" = n)
knitr::kable(count_by_fbs, 
             caption = "Number of patients grouped by fasting blood sugar category.")

################################
# Resting ECG (restecg)
################################

cleveland_heart_disease %>% 
  group_by(restecg,target) %>% 
  summarise(count=n()) %>% 
  mutate(percentage = count/sum(count)) %>%
  ggplot(aes(restecg, y = percentage, fill = target)) +
  geom_bar(stat = "identity") + 
  ggtitle(label = "Resting ECG", subtitle = " ") +
  xlab(NULL) +
  scale_y_continuous(name = "Percentage with / without Heart Disease", 
                     labels = label_percent()) +
  scale_fill_manual(name = "Legend", values = c("darkseagreen3", "indianred2")) +
  theme(legend.position = "right")

count_by_restecg <- cleveland_heart_disease %>% 
  count(restecg) %>% 
  rename("Resting ECG" = restecg, "Count" = n)
knitr::kable(count_by_restecg, 
             caption = "Number of patients grouped by resting ECG category.")


################################
# Maximum Heart Rate Achieved (thalach)
################################

cleveland_heart_disease %>%
  ggplot(aes(thalach, fill = target)) +
  geom_density(alpha = 0.4) +
  ggtitle(label = "Maximum Heart Rate Achieved (in beats per minute)", 
          subtitle = " ") +
  xlab(NULL) +
  ylab("Density") + 
  scale_fill_manual(name="Legend", values = c("darkseagreen3", "indianred2")) + 
  theme(legend.position = "right")


################################
# Exercise-induced angina (exang)
################################

cleveland_heart_disease %>% 
  group_by(exang,target) %>% 
  summarise(count=n()) %>% 
  mutate(percentage = count/sum(count)) %>%
  ggplot(aes(exang, y = percentage, fill = target)) +
  geom_bar(stat = "identity") + 
  ggtitle(label = "Exercise-Induced Angina", subtitle = " ") +
  xlab(NULL) +
  scale_y_continuous(name = "Percentage with / without Heart Disease", 
                     labels = label_percent()) +
  scale_fill_manual(name = "Legend", values = c("darkseagreen3", "indianred2")) +
  theme(legend.position = "right")

count_by_exang <- cleveland_heart_disease %>% 
  count(exang) %>% 
  rename("Exercise-Induced Angina" = exang, "Count" = n)
knitr::kable(count_by_exang, 
             caption = "Number of patients grouped by exercise-induced angina category.")


################################
# ST depression induced by exercise relative to rest (oldpeak)
################################

cleveland_heart_disease %>%
  ggplot(aes(oldpeak, fill = target)) +
  geom_density(alpha = 0.4) +
  ggtitle(label = "ST Depression Induced by Exercise Relative to Rest", 
          subtitle = " ") +
  xlab(NULL) +
  ylab("Density") + 
  scale_fill_manual(name="Legend", values = c("darkseagreen3", "indianred2")) + 
  theme(legend.position = "right")


################################
# Slope of peak exercise ST segment (slope)
################################

cleveland_heart_disease %>% 
  group_by(slope,target) %>% 
  summarise(count=n()) %>% 
  mutate(percentage = count/sum(count)) %>%
  ggplot(aes(slope, y = percentage, fill = target)) +
  geom_bar(stat = "identity") + 
  ggtitle(label = "Slope of Peak Exercise ST Segment", subtitle = " ") +
  xlab(NULL) +
  scale_y_continuous(name = "Percentage with / without Heart Disease", 
                     labels = label_percent()) +
  scale_fill_manual(name = "Legend", values = c("darkseagreen3", "indianred2")) +
  theme(legend.position = "right")

count_by_slope <- cleveland_heart_disease %>% 
  count(slope) %>% 
  rename("Slope of Peak Exercise ST Segment" = slope, "Count" = n)
knitr::kable(count_by_slope, 
             caption = "Number of patients grouped by slope of peak exercise ST segment.")


################################
# Number of Major Vessels Colored by Fluoroscopy (ca)
################################

cleveland_heart_disease %>%
  ggplot(aes(ca, fill = target)) +
  geom_density(alpha = 0.4) +
  ggtitle(label = "Number of Major Vessels Colored by Fluoroscopy", subtitle = " ") +
  xlab(NULL) +
  ylab("Density") + 
  scale_fill_manual(name="Legend", values = c("darkseagreen3", "indianred2")) + 
  theme(legend.position = "right")


################################
# Thallium stress test result (thal)
################################

cleveland_heart_disease %>% 
  group_by(thal,target) %>% 
  summarise(count=n()) %>% 
  mutate(percentage = count/sum(count)) %>%
  ggplot(aes(thal, y = percentage, fill = target)) +
  geom_bar(stat = "identity") + 
  ggtitle(label = "Thallium Stress Test Result", subtitle = " ") +
  xlab(NULL) +
  scale_y_continuous(name = "Percentage with / without Heart Disease", 
                     labels = label_percent()) +
  scale_fill_manual(name = "Legend", values = c("darkseagreen3", "indianred2")) +
  theme(legend.position = "right")

count_by_thal <- cleveland_heart_disease %>% 
  count(thal) %>% 
  rename("Thallium Stress Test Result" = thal, "Count" = n)
knitr::kable(count_by_thal, 
             caption = "Number of patients grouped by thallium stress test result.")


#########################################################################################
# Set up the objects required for the modelling.
#########################################################################################

# Create training and test sets from the "cleveland_heart_disease" set.

# The following line of code assumes that R 3.6 or later is being used.
set.seed(1, sample.kind = "Rounding") 

heart_test_index <- createDataPartition(y = cleveland_heart_disease$target, 
                                        times = 1, p = 0.5, list = FALSE)
heart_train_set <- cleveland_heart_disease %>% slice(-heart_test_index)
heart_test_set <- cleveland_heart_disease %>% slice(heart_test_index)


# Display the structure of the new training set.
str(heart_train_set)

# Display the structure of the new test set.
str(heart_test_set)


#########################################################################################
# Apply different models to try to predict whether or not the patient has heart disease.
#########################################################################################

# Generalized Linear Model (glm)
fit_glm <- train(target ~ ., method ="glm", data = heart_train_set)
y_hat_glm <- predict(fit_glm, heart_test_set, type = "raw")
cm_glm <- confusionMatrix(data = y_hat_glm, reference = heart_test_set$target)

model_results <- tibble(Method = "Generalized Linear Model", 
                        Accuracy = cm_glm$overall[["Accuracy"]],
                        Sensitivity = cm_glm$byClass[["Sensitivity"]],
                        Specificity = cm_glm$byClass[["Specificity"]])

# Linear Discriminant Analysis (lda)
fit_lda <- train(target ~ ., method ="lda", data = heart_train_set)
y_hat_lda <- predict(fit_lda, heart_test_set, type = "raw")
cm_lda <- confusionMatrix(data = y_hat_lda, reference = heart_test_set$target)

model_results <- bind_rows(model_results, 
                           tibble(Method="Linear Discriminant Analysis",
                                  Accuracy = cm_lda$overall[["Accuracy"]],
                                  Sensitivity = cm_lda$byClass[["Sensitivity"]],
                                  Specificity = cm_lda$byClass[["Specificity"]]))

# k-Nearest Neighbors (knn)
fit_knn <- train(target ~ ., method ="knn", data = heart_train_set)
y_hat_knn <- predict(fit_knn, heart_test_set, type = "raw")
cm_knn <- confusionMatrix(data = y_hat_knn, reference = heart_test_set$target)

model_results <- bind_rows(model_results, 
                           tibble(Method="k-Nearest Neighbors",
                                  Accuracy = cm_knn$overall[["Accuracy"]],
                                  Sensitivity = cm_knn$byClass[["Sensitivity"]],
                                  Specificity = cm_knn$byClass[["Specificity"]]))

# Classification and Regression Tree (rpart)
fit_rpart <- train(target ~ ., method ="rpart", data = heart_train_set)
y_hat_rpart <- predict(fit_rpart, heart_test_set, type = "raw")
cm_cart <- confusionMatrix(data = y_hat_rpart, reference = heart_test_set$target)

model_results <- bind_rows(model_results, 
                           tibble(Method="Classification and Regression Tree",
                                  Accuracy = cm_cart$overall[["Accuracy"]],
                                  Sensitivity = cm_cart$byClass[["Sensitivity"]],
                                  Specificity = cm_cart$byClass[["Specificity"]]))                                  

# Random Forest (rf)
fit_rf <- train(target ~ ., method ="rf", data = heart_train_set)
y_hat_rf <- predict(fit_rf, heart_test_set, type = "raw")
cm_rf <- confusionMatrix(data = y_hat_rf, reference = heart_test_set$target)

model_results <- bind_rows(model_results, 
                           tibble(Method="Random Forest",
                                  Accuracy = cm_rf$overall[["Accuracy"]],
                                  Sensitivity = cm_rf$byClass[["Sensitivity"]],
                                  Specificity = cm_rf$byClass[["Specificity"]]))                                  


#########################################################################################
# Display the results from the modelling.
#########################################################################################

# Display the results from all of the models.
knitr::kable(model_results)

# Display the models sorted in descending order of accuracy.
knitr::kable(model_results %>% arrange(desc(Accuracy)))



