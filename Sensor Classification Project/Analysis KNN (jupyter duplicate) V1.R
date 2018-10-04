library(dplyr)
library(caret)
library(factoextra)
library(class)
library(doMC) 
library(tidyr)


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

fviz_eig(PCA_fitted, addlabels = TRUE)

PCA_scores = as.data.frame(cbind(joined[,1], PCA_fitted$x[,1:8]))
PCA_scores = 
  PCA_scores %>%
  rename(activity = V1)

# Training data
KNN_train =
  PCA_scores %>%
  filter(!is.na(activity)) 

# Test data
KNN_test = 
  PCA_scores %>%
  filter(is.na(activity))

library(doMC) 
registerDoMC(cores = 8) 


KNN_train$activity = as.character(KNN_train$activity)
KNN_train$PC1 = as.numeric(as.character(KNN_train$PC1))
KNN_train$PC2 = as.numeric(as.character(KNN_train$PC2))
KNN_train$PC3 = as.numeric(as.character(KNN_train$PC3))
KNN_train$PC4 = as.numeric(as.character(KNN_train$PC4))
KNN_train$PC5 = as.numeric(as.character(KNN_train$PC5))
KNN_train$PC6 = as.numeric(as.character(KNN_train$PC6))
KNN_train$PC7 = as.numeric(as.character(KNN_train$PC7))
KNN_train$PC8 = as.numeric(as.character(KNN_train$PC8))


trControl = trainControl(method  = "repeatedcv",
                          number  = 7,
                          allowParallel = T,
                          repeats = 8)

KNN_fit = train(activity ~ .,
             method     = "knn",
             tuneGrid   = expand.grid(k = 1:30),
             trControl  = trControl,
             metric     = "Accuracy",
             data       = KNN_train)

KNN_fit
plot(KNN_fit)

# Change dtypes
KNN_test$activity = as.character(KNN_test$activity)
KNN_test$PC1 = as.numeric(as.character(KNN_test$PC1))
KNN_test$PC2 = as.numeric(as.character(KNN_test$PC2))
KNN_test$PC3 = as.numeric(as.character(KNN_test$PC3))
KNN_test$PC4 = as.numeric(as.character(KNN_test$PC4))
KNN_test$PC5 = as.numeric(as.character(KNN_test$PC5))
KNN_test$PC6 = as.numeric(as.character(KNN_test$PC6))
KNN_test$PC7 = as.numeric(as.character(KNN_test$PC7))
KNN_test$PC8 = as.numeric(as.character(KNN_test$PC8))

# Get predictions
prediction = knn(KNN_train[-1], KNN_test[-1], KNN_train[,1], 9)
KNN_test$activity = prediction

# Bind it with previous stuff
myData = cbind(KNN_test[1], KNN_unnecessary_test)

# Pad to 2 digits
myData$exp_id = sprintf("%02d", myData$exp_id)
myData$user_id = sprintf("%02d", myData$user_id)

# Get data
myData %>%
  mutate(user_id = paste("user", user_id, sep=""), exp_id = paste("exp", exp_id, sep="")) %>%
  unite(Id, user_id, exp_id, sample) %>%
  select(Id, Predicted = activity) %>%
  mutate(Predicted = Predicted) %>%
  write_csv("test_set_predictions_p8_k9.csv")






