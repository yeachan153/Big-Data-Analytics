# Discovering many components of PCA are best suited

"
Loading in the data - this process is the same

1) Load libraries
2) Load data we need
"
library(dplyr)
library(caret)
library(factoextra)
library(class)
library(doMC) 
library(tidyr)
library(purrr)

# Training paths
a_train_path = "/home/yeachan153/Desktop/BDS/R/Big Data Analytics/Big-Data-Analytics/Sensor Classification Project/Dataset/all_raw_data_a_train.csv"
g_train_path = "/home/yeachan153/Desktop/BDS/R/Big Data Analytics/Big-Data-Analytics/Sensor Classification Project/Dataset/all_raw_data_g_train.csv"
a_test_path = "/home/yeachan153/Desktop/BDS/R/Big Data Analytics/Big-Data-Analytics/Sensor Classification Project/Dataset/all_raw_data_a_test.csv"
g_test_path = "/home/yeachan153/Desktop/BDS/R/Big Data Analytics/Big-Data-Analytics/Sensor Classification Project/Dataset/all_raw_data_g_test.csv"

# Training data
data_a_train = read.csv(a_train_path, encoding = "UTF-8")
data_g_train = read.csv(g_train_path, encoding = "UTF-8")

# Testing data
data_a_test = read.csv(a_test_path, encoding = "UTF-8")
data_g_test = read.csv(g_test_path, encoding = "UTF-8")

data = merge(data_a_train, data_g_train,
             by = c("user_id","exp_id","epoch"))

test_data = merge(data_a_test, data_g_test,
                  by = c("user_id", "exp_id", "epoch"))

data = na.omit(data)

data = 
  data %>%
  select(user_id:AR12.1.x,
         m1.y:AR12.1.y) %>%
  rename(activity = activity.x,
         sample = sample.x)

test_data = 
  test_data %>%
  select(user_id:AR12.1.x,
         m1.y:AR12.1.y) %>%
  rename(activity = activity.x,
         sample = sample.x)

data$activity = as.character(data$activity)
test_data$activity = as.character(test_data$activity)

test_data$activity = NA

# Get unnecessary sections for train/test
KNN_unnecessary = 
  data %>%
  select(user_id:epoch,
         sample)

KNN_unnecessary_test = 
  test_data %>%
  select(user_id:epoch,
         sample)

# Get necessary sections for train/test
KNN_train = 
  data %>%
  select(-user_id:-epoch,
         -sample)

KNN_test = 
  test_data %>%
  select(-user_id:-epoch,
         -sample)

joined = rbind(KNN_train, KNN_test)

# PCA data 
PCA_data = 
  joined %>%
  select(-activity)

# PCA fitted
PCA_fitted = prcomp(PCA_data, scale = TRUE)

"
1) Create a list that contains PCA results for principle components from 4-20
2) Split the list into training data and test data by filtering on NA activities
"
num_dimensions = 1:20
df_list = list()

for (current_dimension in num_dimensions) {
  
  # Get PCA score for current number of dimensions and join with activity
  PCA_scores = as.data.frame(cbind(joined[,1], PCA_fitted$x[,1:current_dimension]))
  
  # Rename V1 to activity 
  PCA_scores = 
    PCA_scores %>%
    rename(activity = V1) %>%
    mutate_each(funs(as.character), starts_with("PC")) %>%
    mutate_each(funs(as.numeric), starts_with("PC")) %>%
    mutate(activity = as.character(activity))
  
  # Put each dataframe in a list, the key to each list the number of dimensions
  df_list[[current_dimension]] = PCA_scores
}

# Returns a list containing training PCA scores
KNN_train_list = function(df) {
  df = 
    df %>%
    filter(!is.na(activity))
  return(df)
}

# Returns a list containing test PCA scores
KNN_test_list = function(df) {
  df = 
    df %>%
    filter(is.na(activity))
  return(df)
}

KNN_train = map(df_list, KNN_train_list)
KNN_test = map(df_list, KNN_test_list)

" Run the process for different values of principle components"
# Enable parallel processing
library(doMC) 
registerDoMC(cores = 8) 

fits = list()
plots = list()
total_dimensions = length(df_list)

# Define cross validation parameters - repeated, 
trControl = trainControl(method  = "repeatedcv",
                         number  = 7,
                         allowParallel = T,
                         repeats = 8)


for (current_dimension in 1:total_dimensions) {
  
  
}

KNN_fit = train(activity ~ .,
                method     = "knn",
                tuneGrid   = expand.grid(k = 1:30),
                trControl  = trControl,
                metric     = "Accuracy",
                data       = KNN_train)








