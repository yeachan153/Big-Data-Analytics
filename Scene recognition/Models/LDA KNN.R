"
This model has Kaggle score around 0.758

The basic idea here is to create a PCA-discriminant analysis model.
For classifications where the posterior probabilities for this model are too low, fall 
back to the KNN-PCR model. 

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
source("/home/yeachan153/Desktop/BDS/R/Big Data Analytics/Big-Data-Analytics/Scene recognition/Models/KNN Model.R")

### 2) Load libraries
library(MASS)
library(klaR)

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
Modelling

1) Get data to use for lda
2) Get lda training data
3) Get lda test data
4) Perform LDA backward selection
5) Select training LDA based on selected PCs
6) Select test LDA based on selected PCs
7) Fit the model
8) Filter predictions with high posterior probabilities
9) Get high posterior predictions from LDA, the rest from KNN
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
lda_backwardselection = stepclass(lda_data_train %>% dplyr::select(-file,-category),
                                   grouping=lda_data_train$category, 
                                   method='lda', direction='backward') 

### 5) Select training LDA based on selected PCs
lda_data_train = 
lda_data_train %>%
  dplyr::select(file, category, lda_backwardselection$model$name)

### 6) Select test LDA based on selected PCs
lda_data_test = 
  lda_data_test %>%
  dplyr::select(file, category, lda_backwardselection$model$name)

### 7) Fit the model
lda_model = lda(category ~ ., data = lda_data_train %>% dplyr::select(-file))

### 8) Filter predictions with high posterior probabilities
predictions_LDA_raw =
  predict(lda_model, lda_data_test %>% dplyr::select(-file,-category))$posterior %>%
  as.tibble() %>%
  mutate_all(funs(ifelse(. > 0.83, ., NA))) 

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

### 9) Get high posterior predictions from LDA, the rest from KNN
lda_data_test_final = 
  lda_data_test %>%
  dplyr::select(file, category) %>%
  mutate(category = predictions_LDA) %>%
  left_join(test_data_KNN %>% 
              dplyr::select(file, category),
            by = c("file" = "file")) %>%
  mutate(category.y = as.character(category.y),
         category.x = ifelse(is.na(category.x), category.y, category.x)) %>%
  rename(category = category.x) %>%
  dplyr::select(file, category)
  
# path = "/home/yeachan153/Desktop/BDS/R/Big Data Analytics/Big-Data-Analytics/Scene recognition/Predictions/LDA_P73_K7_P10.csv"
# 
# lda_data_test_final %>%
#   dplyr::select(file, category) %>%
#   write.csv(path, fileEncoding = "UTF-8",
#             row.names = F)
# 











