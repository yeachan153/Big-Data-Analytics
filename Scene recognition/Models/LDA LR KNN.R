"
Kaggle score of 0.76884

The basic idea here is to create a PCA-discriminant analysis model.
For classifications where the posterior probabilities for this model are too low, fall 
back to the PCR-multinomial regression model. Finally, if the probabilities here are low,
fall back to the PCR-KNN model.

Source the KNN file to load in some libraries and raw data.

1) Source 'KNN Model.R'
- 'raw_data' contains the raw data (training + test), unstandardised
- 'PCA' contains the return of a prcomp function. Get PCR scores with PCA$x
Use mutate to create the file column with 'raw_data$file' and the category
column with 'raw_data$category'
- test_data_KNN holds the results of the KNN.
2) Load libraries
3) Define needed functions
"
### 1) Source 'KNN Model.R'
library(MASS) 
source("/home/yeachan153/Desktop/BDS/R/Big Data Analytics/Big-Data-Analytics/Scene recognition/Models/KNN Model.R")

### 2) Load libraries
library(klaR)
library(nnet)

### 3) Define needed functions

# Extract predictions, including missing ones to be supplemented by KNN
rowwise_extractor = function(row) {
  if (sum(is.na(row)) == 4) {
    return(NA)
  } else {
    activity = 
      row %>%
      select_if(~sum(!is.na(.)) > 0) %>%
      colnames()
    return(activity)
  }
}

"
Modelling LDA

1) Get data to use for lda
2) Get lda training data
3) Get lda test data
4) Perform LDA backward selection
5) Select training LDA based on selected PCs
6) Select test LDA based on selected PCs
7) Fit the model
8) Filter predictions with high posterior probabilities
9) Create LDA submission format
"
### 1) Get data to use for lda
lda_data = 
  PCA$x %>%
  as.tibble() %>%
  dplyr::select(PC1:PC80) %>%
  mutate(file = raw_data$file,
         category = raw_data$category) %>%
  dplyr::select(file, category, everything())

### 2) Get lda training data
lda_data_train = 
  lda_data %>%
  filter(!is.na(category)) 

### 3) Get lda test data
lda_data_test =
  lda_data %>%
  filter(is.na(category))

### 4) Perform LDA backward selection
# lda_backwardselection = stepclass(lda_data_train %>% dplyr::select(-file,-category),
#                                   grouping=lda_data_train$category, 
#                                   method='lda', direction='backward') 

# Results of backward selection:

# PC1,PC2,PC3,PC4,
# PC5,PC8,PC9,PC10,
# PC12,PC13,PC14,PC15,
# PC16,PC17,PC18,PC19,PC20,
# PC21,PC22,PC23,PC24,PC26,
# PC27,PC28,PC29,PC31,PC32,
# PC33,PC35,PC36,PC37,PC38,PC39,
# PC40,PC41,PC42,PC43,PC44,PC45,
# PC47,PC48,PC49,PC50,PC51,
# PC52,PC53,PC54,PC55,PC56,
# PC57,PC58,PC60,PC61,PC62,
# PC63,PC65,PC66,PC67,PC68,
# PC69,PC70,PC74,PC75,PC77,PC80


### 5) Select training LDA based on selected PCs
lda_data_train = 
  lda_data_train %>%
  dplyr::select(file, category, 
                PC1,PC2,PC3,PC4,
                PC5,PC8,PC9,PC10,
                PC12,PC13,PC14,PC15,
                PC16,PC17,PC18,PC19,PC20,
                PC21,PC22,PC23,PC24,PC26,
                PC27,PC28,PC29,PC31,PC32,
                PC33,PC35,PC36,PC37,PC38,PC39,
                PC40,PC41,PC42,PC43,PC44,PC45,
                PC47,PC48,PC49,PC50,PC51,
                PC52,PC53,PC54,PC55,PC56,
                PC57,PC58,PC60,PC61,PC62,
                PC63,PC65,PC66,PC67,PC68,
                PC69,PC70,PC74,PC75,PC77,PC80)

### 6) Select test LDA based on selected PCs
lda_data_test = 
  lda_data_test %>%
  dplyr::select(file, category, 
                PC1,PC2,PC3,PC4,
                PC5,PC8,PC9,PC10,
                PC12,PC13,PC14,PC15,
                PC16,PC17,PC18,PC19,PC20,
                PC21,PC22,PC23,PC24,PC26,
                PC27,PC28,PC29,PC31,PC32,
                PC33,PC35,PC36,PC37,PC38,PC39,
                PC40,PC41,PC42,PC43,PC44,PC45,
                PC47,PC48,PC49,PC50,PC51,
                PC52,PC53,PC54,PC55,PC56,
                PC57,PC58,PC60,PC61,PC62,
                PC63,PC65,PC66,PC67,PC68,
                PC69,PC70,PC74,PC75,PC77,PC80)

### 7) Fit the model
lda_model = lda(category ~ ., data = lda_data_train %>% dplyr::select(-file))

### 8) Filter predictions with high posterior probabilities
predictions_LDA_raw =
  predict(lda_model, lda_data_test %>% dplyr::select(-file,-category))$posterior %>%
  as.tibble() %>%
  mutate_all(funs(ifelse(. > 0.81, ., NA))) 

predictions_LDA = c()

# Get predictions
for (current_row in 1:nrow(predictions_LDA_raw)) {
  # Get current row
  current_row = predictions_LDA_raw[current_row,]
  # Get predictions
  predictions_current_row = rowwise_extractor(current_row)
  # Append to predictions of LDA
  predictions_LDA = append(predictions_LDA, predictions_current_row)
}

### 9) Create LDA submission format
LDA_submission_format=
  lda_data_test %>%
  select(file, category) %>%
  mutate(category = predictions_LDA)

"
Multinomial model
1) Get data to use for MLR
2) Get MLR training data
3) Get MLR test data
4) Train the model
5) Get raw predictions
6) Get predictions above threshold
7) Get MLR in submission format

"
### 1) Get data to use for MLR
MLR_data = 
  PCA$x %>%
  as.tibble() %>%
  dplyr::select(PC1:PC80) %>%
  mutate(file = raw_data$file,
         category = raw_data$category) %>%
  dplyr::select(file, category, everything())

### 2) Get MLR training data
MLR_data_train = 
  MLR_data %>%
  filter(!is.na(category)) 

### 3) Get MLR test data
MLR_data_test =
  MLR_data %>%
  filter(is.na(category))

### 4) Train the model
MLR_data_train = 
  MLR_data_train %>%
  select(-file) %>%
  mutate(category = factor(MLR_data_train$category))

multi_model = multinom(category ~ ., data = MLR_data_train %>%
                                              select(category, PC1:PC20))
### 5) Get raw predictions
multinom_predict_raw = 
  predict(multi_model, 
          MLR_data_test %>%
          select(-file, -category),
          type = "prob") %>%
  as.tibble() %>%
  mutate_all(funs(ifelse(. > 0.925, ., NA))) 

### 6) Get predictions above threshold
predictions_MLR = c()

# Get predictions
for (current_row in 1:nrow(multinom_predict_raw)) {
  # Get current row
  current_row = multinom_predict_raw[current_row,]
  # Get predictions
  predictions_current_row = rowwise_extractor(current_row)
  # Append to predictions of LDA
  predictions_MLR = append(predictions_MLR, predictions_current_row)
}

### 7) Get MLR in submission format
MLR_submission_format = 
  MLR_data_test %>%
  select(file, category) %>%
  mutate(category = predictions_MLR)

"
Join LDA -> MLR -> KNN

1) Supplement LDA with MLR if LDA threshold low, MLR threshold high
2) Supplement LDA/MLR model with KNN if MLR threshold low
3) Write to .csv submission format
"
### 1) Supplement LDA with MLR if LDA threshold low, MLR threshold high
LDA_LR_KNN_submission_format = 
  LDA_submission_format %>%
  left_join(MLR_submission_format, 
            by = c("file" = "file")) %>%
  mutate(category.x = ifelse(is.na(category.x), category.y, category.x)) %>%
  select(-category.y) %>%
### 2) Supplement LDA/MLR model with KNN if MLR threshold low
  left_join(test_data_KNN %>%
              select(file, category) %>%
              mutate(category = as.character(category)), 
            by = c("file" = "file")) %>%
  mutate(category.x = ifelse(is.na(category.x), category, category.x)) %>%
  select(-category) %>%
  rename(category = category.x)

### 3) Write to .csv submission format
# path = "/home/yeachan153/Desktop/BDS/R/Big Data Analytics/Big-Data-Analytics/Scene recognition/Predictions/LDA_P73_LR_73_KNN_K7.csv"
# 
# LDA_LR_KNN_submission_format %>%
#   write.csv(path, fileEncoding = "UTF-8",
#             row.names = F)








