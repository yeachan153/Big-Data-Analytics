"
This model has a cross validated classification error - 0.74
The basic idea is to reduce dimensions with PCA and then perform KNN

Set up the R Script

1) Load libraries
2) Set parallel processing setting
3) Set Path
4) Read in CSV
"
### 1) Load libraries
library(dplyr)
library(tidyr)
library(tibble)
library(factoextra)
library(doMC) 
library(caret)
library(class)

### 2) Set parallel processing setting
registerDoMC(cores = 8) 

### 3) Set path
path = "/home/yeachan153/Desktop/BDS/R/Big Data Analytics/Big-Data-Analytics/Scene recognition/Train_V1.csv"

### 4) Read in CSV
raw_data = read.csv(path, 
                encoding = "UTF-8",
                stringsAsFactors = F) %>%
           as.tibble()

"
Process the data

1) Standardise the entire dataset to z scores and perform PCA
2) Show PCA plot explaining variance
3) Get pruned PCA dataframe
4) Get test data
5) Get training data
"
### 1) Z Score standardisation and PCA 
PCA =  
  raw_data %>%
  select(-file, -category) %>%
  scale() %>%
  as.tibble() %>%
  prcomp(scale = F)

### 2) Show PCA plot explaining variance
fviz_eig(PCA)

### 3) Get pruned PCA dataframe
pruned_PCA = 
  PCA$x %>%
  as.tibble() %>%
  mutate(file = raw_data$file,
         category = raw_data$category) %>%
  select(file, category, everything()) %>%
  select(file:PC10)

### 4) Get test data
test_data_KNN = 
  pruned_PCA %>%
  filter(is.na(category))

### 5) Get training data 
train_data_KNN =
  pruned_PCA %>%
  filter(!is.na(category))
 
"
Modelling and getting predictions

1) Set cross validation parameters
2) Perform cross validation to determine K for KNN
3) Plot the cross validation - we pick K of 7
4) Get predicted values
"
### 1) Set cross validation parameters
trControl = trainControl(method  = "repeatedcv",
                         number  = 7,
                         allowParallel = T,
                         repeats = 10)

### 2)  Perform cross validation to determine K for KNN
KNN_fit = train(category ~ .,
                method     = "knn",
                tuneGrid   = expand.grid(k = 1:30),
                trControl  = trControl,
                metric     = "Accuracy",
                data       = train_data_KNN %>% select(-file))

### 3) Plot cross validation
plot(KNN_fit)

### 4) Get predicted values
predicted_categories = 
  class::knn(train_data_KNN %>% select(-file, -category), 
             test_data_KNN %>% select(-file, - category), 
             train_data_KNN$category, 7)

test_data_KNN = 
  test_data_KNN %>%
  mutate(category = predicted_categories)

"
Convert to submission format

1) Create submission csv
"
### 1) Create submission csv
# path = "/home/yeachan153/Desktop/BDS/R/Big Data Analytics/Big-Data-Analytics/Scene recognition/Predictions/KNN_K7.csv"
# 
# test_data %>% 
#   select(file, category) %>% 
#   write.csv(path, fileEncoding = "UTF-8",
#             row.names = F)




