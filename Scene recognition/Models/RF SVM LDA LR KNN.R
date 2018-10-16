"
Get predictions from all models - choose the most common one predicted.

1) Load libraries
2) Load RF predictions
3) Load KNN predictions
4) Load MLR predictions
5) Load LDA predictions
6) Load SVM predictions
7) Join all in a common tibble
"
### 1) Load libraries
library(MASS)
library(dplyr)
library(tibble)

### 2) Load RF predictions
RF = 
  read.csv("/home/yeachan153/Desktop/BDS/R/Big Data Analytics/Big-Data-Analytics/Scene recognition/Predictions/Kevin RF Vanilla.csv",
         stringsAsFactors = F,
         fileEncoding = "UTF-8") %>%
  as.tibble() %>%
  rename(RF = category)

### 3) Load KNN predictions
source("/home/yeachan153/Desktop/BDS/R/Big Data Analytics/Big-Data-Analytics/Scene recognition/Models/KNN Model.R")

KNN = 
  test_data_KNN %>%
  select(file, category) %>%
  rename(KNN = category) %>%
  mutate(KNN = as.character(KNN))

### 4) Load MLR predictions
source("/home/yeachan153/Desktop/BDS/R/Big Data Analytics/Big-Data-Analytics/Scene recognition/Models/LDA LR KNN.R")

MLR = 
  MLR_data_test %>%
  mutate(category = 
           predict(multi_model, 
                   MLR_data_test %>%
                   select(-file, -category))) %>%
  select(file, category) %>%
  rename(MLR = category) %>%
  mutate(MLR = as.character(MLR))

### 5) Load LDA predictions
LDA = 
  lda_data_test %>%
  mutate(category = predict(lda_model, 
                            lda_data_test %>% 
                              dplyr::select(-file,-category))$class) %>%
  select(file, category) %>%
  rename(LDA = category) %>%
  mutate(LDA = as.character(LDA))

### 6) Load SVM predictions
source("/home/yeachan153/Desktop/BDS/R/Big Data Analytics/Big-Data-Analytics/Scene recognition/Models/SVM LDA LR KNN.R")

SVM =
  PC13_svm_test %>%
  mutate(category = predict(svm_radial2,
                            PC13_svm_test %>%
                              select(-file, -category))) %>%
  select(file, category) %>%
  rename(SVM = category) %>%
  mutate(SVM = as.character(SVM))

### 7) Join all in a common tibble
combined = 
  RF %>%
  left_join(SVM, by = c("file" = "file")) %>%
  left_join(LDA, by = c("file" = "file")) %>%
  left_join(MLR, by = c("file" = "file")) %>%
  left_join(KNN, by = c("file" = "file"))


"
Develop Logic

1) When the 4 other models disagree with RF, and the other 4 models
   agree perfectly, we change the RF predictions to match. There
   are 10 such cases. I predict this will take our Kaggle score from 
   0.788 - 0.804.

2) We change the RF predictions to the most popular prediction
   outputter by the 5 models.
"

### 1) 
combined2 = 
  combined %>%
  mutate(certain_difference =
           ifelse((SVM == LDA) & 
                    (LDA == MLR) & 
                    (MLR == KNN) &
                    (RF != SVM), TRUE, FALSE),
         RF = ifelse(certain_difference == T,
                     SVM,
                     RF)) %>%
  select(-certain_difference) 

combined2_submission = 
  combined2 %>%
  select(file, RF) %>%
  rename(category = RF)

### 2)
combined3 = 
  combined2 %>%
  select(RF:KNN)

most_pop = c()

for (i in 1:nrow(combined3)) {
  current_row = combined3[i,]
  pop = names(sort(table(as.vector(unlist(current_row))), decreasing = T)[1])
  most_pop = append(most_pop, pop)
}

combined3 = 
  combined3 %>%
  mutate(pop = most_pop,
         RF = ifelse(RF != pop, pop, RF)) 

combined3_submission =
  combined3 %>%
  mutate(file = combined2$file) %>%
  rename(category = RF) %>%
  select(file, category)

"
Write to CSV

1) combined2_submission
2) combined3_submission
"

### 1) combined2_submission
path = "/home/yeachan153/Desktop/BDS/R/Big Data Analytics/Big-Data-Analytics/Scene recognition/Predictions/RF_SVD_P13_LDA_P73_MLR_P73_KNN_K7_logic_V1_Vanilla.csv"

combined2_submission %>%
  write.csv(path, fileEncoding = "UTF-8",
            row.names = F)

### 1) combined3_submission
path = "/home/yeachan153/Desktop/BDS/R/Big Data Analytics/Big-Data-Analytics/Scene recognition/Predictions/RF_SVD_P13_LDA_P73_MLR_P73_KNN_K7_logic_V2_Vanilla.csv"

combined3_submission %>%
  write.csv(path, fileEncoding = "UTF-8",
            row.names = F)




