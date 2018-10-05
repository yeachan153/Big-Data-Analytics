"
What is the point of this script?

We leverage the power of LDA and KNN together. We use the classifications from LDA when the 
probability for that class of activity is high. When it is relatively low, we discard the 
classifications produced by the LDA and rely on a KNN classifier.

How?

We create 2 separate models that use different features - an LDA classifier (Sophie's model 6) and a 
KNN classifier. We make these 2 models independently as good as possible. Then, we predict what we 
can with the LDA classifier with a high degree of certainty, and leave the rest for the KNN to mop up. 
More technically, we look at the posterior probabilities given by the LDA for each class, and use a
cut-off point at which we reject predictions from the LDA and fall back on KNN.

Steps:

1) Make the optimal KNN model with features (9 simple features only).
2) Use the optimal LDA model with features (Use model 6 from Sophie - 97 features).
3) Get probability table from Logistic regression
4) Supplement LDA predictions with low posterior probability with KNN

This framework was tested on Kaggle and achieved 0.728. However, this was a rough first attempt at 1am. There
are three ways in which it can be improved:

1) Improve the LDA model (note that LDA and KNN don't have to use the same features!) 
2) Play around with the posterior probability cut off at which predictions from LDA are discarded, and KNN
predictions are used instead (line 151). Currently, it's at 0.9 - therefore predictions from LDA with 
posterior probabilities of less than 0.9 are discarded. This yields around 1800 LDA classifications and 
700 KNN classifications.
3) Improve the KNN model (LOWEST PRIORITY - this KNN model was already tested on Kaggle on 04/10/18 and
achieved a score of 0.69. Not worth tampering with at this point because it's near our best submitted
KNN.)

"
library(MASS)
library(caret)
library(tidyr)
library(readr)
library(rknn)
library(doMC) 
library(tibble)
library(dplyr)

# Used for caret parallel processing (multi-core CPU)
registerDoMC(cores = 8) 

"
1) Making the KNN model

a) Load in the data (pre-standardised across training/test set by running 'KNN_dataset maker.R')
b) Use simple features - mean/sd. 9 features selected.
"
##### a) Load in the data
# Or download from GitHub: https://github.com/yeachan153/Big-Data-Analytics/tree/master/Sensor%20Classification%20Project/LDA_KNN%20framework
path_train = "/home/yeachan153/Desktop/BDS/R/Big Data Analytics/Big-Data-Analytics/Sensor Classification Project/LDA_KNN framework/KNN_train.csv"
path_test = "/home/yeachan153/Desktop/BDS/R/Big Data Analytics/Big-Data-Analytics/Sensor Classification Project/LDA_KNN framework/KNN_test.csv"

training_KNN = as.tibble(read.csv(path_train, encoding = "UTF-8", 
                                  stringsAsFactors = F))
test_KNN = as.tibble(read.csv(path_test, encoding = "UTF-8",
                              stringsAsFactors = F))

# Rename columns
training_KNN = 
  training_KNN %>%
  rename(activity = activity.x) %>%
  dplyr::select(activity, m1.x, 
                m2.x, m3.x, 
                sd1.x, sd2.x, 
                sd3.x, m1.y, 
                m2.y, m3.y)

test_KNN = 
  test_KNN %>%
  rename(activity = activity.x,
         sample = sample.x) %>%
  dplyr::select(user_id:sample,
                m1.x, m2.x, 
                m3.x, sd1.x, 
                sd2.x, sd3.x, 
                m1.y, m2.y, m3.y)

# Cross validation parameters
# trControl = trainControl(method  = "repeatedcv",
#                          number  = 7,
#                          allowParallel = T,
#                          repeats = 10)
# 
# KNN_fit = train(activity ~ .,
#                 method     = "knn",
#                 tuneGrid   = expand.grid(k = 1:30),
#                 trControl  = trControl,
#                 metric     = "Accuracy",
#                 data       = training_KNN)

# Get KNN predictions
predictions_KNN =
  class::knn(training_KNN[-1], 
             test_KNN[-1:-5], 
             training_KNN$activity, 9)

myDataKNN = 
  test_KNN %>%
  mutate(activity = predictions_KNN,
         exp_id = sprintf("%02d", test_KNN$exp_id),
         user_id = sprintf("%02d", test_KNN$user_id)) %>%
  dplyr::select(user_id:sample) %>%
  rename(activity_KNN = activity)

# 0.69 result:
# myData %>%
#   mutate(user_id = paste("user", user_id, sep=""), exp_id = paste("exp", exp_id, sep="")) %>%
#   unite(Id, user_id, exp_id, sample) %>%
#   dplyr::select(Id, Predicted = activity) %>%
#   mutate(Predicted = Predicted) 


"
Importing the LDA model
a) Import Sophie's rdata and extract predictions
"
# Download from groupchat.
# GitHub link: https://github.com/yeachan153/Big-Data-Analytics/blob/master/Sensor%20Classification%20Project/Sophie_Workspace.Rdata
load("/home/yeachan153/Desktop/BDS/R/Big Data Analytics/Big-Data-Analytics/Sensor Classification Project/Sophie_Workspace.Rdata")

lda_model = lda(activity.x ~., data = tryout[,c(1,lda.backwardselection.V4$model$nr[c(-19,-39)])])

testdata_lda =
  testdata_new %>%
  as.tibble() %>%
  dplyr::select(user_id:sample,
                colnames(lda_model$means)) 

# Extract predictions, including missing ones to be supplemented by KNN
rowwise_extractor = function(row) {
  if (sum(is.na(row)) == 13) {
    return(NA)
  } else {
    activity = 
      row %>%
      select_if(~sum(!is.na(.)) > 0) %>%
      colnames()
    return(activity)
  }
}

# Make all posterior probabilities under .9 NA.
predictions_LDA_raw = 
  predict(lda_model, testdata_lda[-1:-5])$posterior %>% 
  as.tibble() %>%
  mutate_all(funs(ifelse(. > 0.85, ., NA))) 

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

# Number unclassified
# sum(is.na(predictions_LDA))

"
3) Create multinomial model (Thomas)
"

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

# Probability table
multinom_predict = predict(multi_model, testing_KNN, type = "prob")

testdata_multi_raw =
  as.tibble(multinom_predict) %>%
  mutate_all(funs(ifelse(. > 9, ., NA))) 

# Check how many rows we have predictions for after 0.85 cut off - 1380.
# as.tibble(multinom_predict) %>%
#     mutate_all(funs(ifelse(. > 0.85, ., NA))) %>%
#     filter_all(any_vars(!is.na(.)))
    
predictions_multi = c() 

# Get predictions
for (current_row in 1:nrow(testdata_multi_raw)) {
  # Get current row
  current_row = testdata_multi_raw[current_row,]
  # Get predictions
  predictions_current_row = rowwise_extractor(current_row)
  # Append to predictions of LDA
  predictions_multi = append(predictions_multi, predictions_current_row)
}

  
"
3) Joining LDA and LR and KNN results together
"
# The dataframe with predictions from LDA - some activities are NA
# because posterior probabilities were low
final_test_LDA = 
  testdata_lda %>%
  dplyr::select(user_id:sample) %>%
  mutate(activity = predictions_LDA) 

final_test_multi = 
  testing_KNN %>%
  rename(activity_multi = activity.x,
         sample = sample.x) %>%
  select(user_id:sample) %>%
  mutate(activity_multi = predictions_multi,
         exp_id = sprintf("%02d", test_KNN$exp_id),
         user_id = sprintf("%02d", test_KNN$user_id))
  

# LDA with no missing values (JOIN WITH LATER!!!!)
final_test_LDA_predicted =
  final_test_LDA %>%
  filter(!is.na(activity))

# LDA with missing data
LDA_missing = 
  final_test_LDA %>%
  filter(is.na(activity))

# Filled by LG
LG_fill = 
  LDA_missing %>%
  left_join(final_test_multi, on = c("user_id" = "user_id",
                                     "exp_id" = "exp_id",
                                     "epoch" = "epoch")) %>%
  mutate(activity = activity_multi) %>%
  select(-activity_multi) 

# Only those filled by LG
LG_fill2 = 
  LG_fill %>%
  filter(!is.na(activity))

# Filled with KNN
LDA_LG_KNN = 
  LG_fill %>%
  filter(is.na(activity)) %>%
  left_join(myDataKNN, by = c("user_id" = "user_id",
                              "exp_id" = "exp_id",
                              "epoch" = "epoch",
                              "sample" = "sample")) %>%
  mutate(activity = activity_KNN) %>%
  select(-activity_KNN) %>%
  mutate(activity = as.character(activity)) %>%
  bind_rows(LG_fill2) %>%
  bind_rows(final_test_LDA_predicted) %>%
  arrange(user_id, exp_id, epoch)

# Final csv file
LDA_LG_KNN %>%
  mutate(user_id = paste("user", user_id, sep=""), exp_id = paste("exp", exp_id, sep="")) %>%
  unite(Id, user_id, exp_id, sample) %>%
  dplyr::select(Id, Predicted = activity) %>%
  mutate(Predicted = Predicted) %>%
  write_csv("test_set_predictions_LDA_P0.85_LG_P0.9_KNN_F9_K9.csv")

# # Put everything together with KNN predictions
# LDA_KNN =
#   final_test_LDA %>%
#   filter(is.na(activity)) %>%
#   left_join(myDataKNN, 
#             by = c("user_id" = "user_id",
#                    "exp_id" = "exp_id",
#                    "epoch" = "epoch",
#                    "sample" = "sample")) %>%
#   dplyr::select(-activity) %>%
#   rename(activity = activity_KNN) %>%
#   dplyr::select(user_id, exp_id,
#                 epoch, activity, 
#                 sample) %>%
#   mutate(activity = as.character(activity)) %>%
#   bind_rows(final_test_LDA_predicted) %>%
#   arrange(user_id, exp_id, epoch, sample)

