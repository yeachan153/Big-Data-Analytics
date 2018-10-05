library(nnet)
library(MASS)
library(caret)
library(tidyr)
library(readr)
library(rknn)
library(doMC) 
library(tibble)
library(dplyr)

training_path = "/home/yeachan153/Desktop/BDS/R/Big Data Analytics/Big-Data-Analytics/Sensor Classification Project/LDA_KNN framework/KNN_train.csv"
testing_path = "/home/yeachan153/Desktop/BDS/R/Big Data Analytics/Big-Data-Analytics/Sensor Classification Project/LDA_KNN framework/KNN_test.csv"

training_KNN = as.tibble(read.csv(training_path, encoding = "UTF-8", stringsAsFactors = F))
testing_KNN = as.tibble(read.csv(testing_path, encoding = "UTF-8", stringsAsFactors = F))

# multinomial regression
# variable should be interger
training_KNN$activity.F <- factor(training_KNN$activity.x)
# reference variable 
training_KNN$out <- relevel(training_KNN$activity.F, ref = "-")

multi_model <- multinom(out ~ ., data = training_KNN[, colnames(training_KNN)[c(2:57, 59)]])

multinom_predict = predict(multi_model, testing_KNN, type = "prob")

