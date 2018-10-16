"
Kaggle score of 0.77386

The basic idea here is to create a PCA-SVM model and supplement predictions with LDA.
For classifications where the posterior probabilities for this model are too low, fall 
back to the PCR-multinomial regression model. Finally, if the probabilities here are low,
fall back to the PCR-KNN model.

Source the KNN file to load in some libraries and raw data.

1) Source 'LDA LR KNN.R'
2) Load libraries
3) Get PCA components for SVA
4) Run SVA cross validation
5) Get raw predictions above threshold
6) Get submittable file
"
### 1) 
source("/home/yeachan153/Desktop/BDS/R/Big Data Analytics/Big-Data-Analytics/Scene recognition/Models/LDA LR KNN.R")

### 2) Load libraries
library(caret)

### 3) Get PCA components for SVA
PC13 =
  PCA$x %>%
  as.tibble() %>%
  mutate(file = raw_data$file,
         category = raw_data$category) %>%
  select(file, category, everything()) %>%
  select(file:PC13)

# Training data
PC13_svm_train = 
  PC13 %>%
  filter(!is.na(category)) 

# Test data
PC13_svm_test = 
  PC13 %>%
  filter(is.na(category))

# Multicore processing
registerDoMC(cores = 8)


### 4) Run SVA cross validation
trControl = trainControl(method  = "repeatedcv",
                         number  = 7,
                         allowParallel = T,
                         repeats = 10,
                         classProbs = T)

trControl2 = trainControl(method  = "repeatedcv",
                         number  = 7,
                         allowParallel = T,
                         repeats = 10,
                         classProbs = F)

svm_radial = train(category ~ .,
                   method     = "svmRadial",
                   trControl  = trControl,
                   tuneLength = 10,
                   data       = PC13_svm_train %>% select(-file))

svm_radial2 = train(category ~ .,
                   method     = "svmRadial",
                   trControl  = trControl2,
                   tuneLength = 10,
                   data       = PC13_svm_train %>% select(-file))

### 5) Get raw predictions above threshold
svm_predictions_raw = 
  predict(svm_radial, 
        PC13_svm_test %>% 
          select(-file, -category), type = "prob") %>%
  as.tibble() %>%
  mutate_all(funs(ifelse(. > 0.85, ., NA))) 

predictions_SVM = c()

# Get predictions
for (current_row in 1:nrow(svm_predictions_raw)) {
  # Get current row
  current_row = svm_predictions_raw[current_row,]
  # Get predictions
  predictions_current_row = rowwise_extractor(current_row)
  # Append to predictions of LDA
  predictions_SVM = append(predictions_SVM, predictions_current_row)
}

### 6) Get submittable file
SVM_LDA_MLR_KNN_submission_format = 
  PC13_svm_test %>%
  select(file, category) %>%
  mutate(category = predictions_SVM) %>%
  left_join(LDA_LR_KNN_submission_format, 
            by = c("file" = "file")) %>%
  mutate(category.x = ifelse(is.na(category.x), category.y, category.x)) %>%
  select(-category.y) %>%
  rename(category = category.x)

# path = "/home/yeachan153/Desktop/BDS/R/Big Data Analytics/Big-Data-Analytics/Scene recognition/Predictions/SVM_P13_LDA_P73_LR_73_KNN_K7.csv"
# 
# SVM_LDA_MLR_KNN_submission_format %>%
#   write.csv(path, fileEncoding = "UTF-8",
#             row.names = F)

