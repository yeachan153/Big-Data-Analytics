# set working directory
setwd("~/Dropbox/UvA/Year 2 Sem 1/Big Data Analytics/Competition 3 - Customer satisfaction from review")

# load packages
library(tidyverse)
library(tidytext)
library(glmnet)
library(dplyr)

# load dataset
amazon <- read_csv("amazon_baby.csv") # contains both train and test data
#View(amazon)

# train dataset
trainidx <- !is.na(amazon$rating)
#table(trainidx)
# 153,531 as train set and 30,000 as test set

# break up text into separate tokens and count the number of occurences per review
reviews <- amazon %>% 
  mutate(id = row_number()) %>%
  unnest_tokens(token, review) %>%
  count(id, name, rating, token)

# stopwords counts
sw <- reviews %>% 
  inner_join(get_stopwords(), by = c(token='word')) %>% 
  group_by(id) %>% 
  mutate(N = sum(n))

# check the relationship between ratings and the stopwords frequency and count
#lm(rating ~ n + N, sw) %>% summary # highly significant! 
# cannot remove the stopwords 

# deriving FEATURES
features <- 
  reviews %>%
  group_by(id) %>%
  mutate(nwords = sum(n)) %>% # the number of tokens per document
  group_by(token) %>%
  mutate(
    docfreq = n(), # number of documents that contain the token
    idf = log(nrow(amazon) / docfreq), # inverse document frequency ('surprise')
    tf = n / nwords,  # relative frequency of token within the given document
    tf_idf = tf*idf
  ) %>%
  ungroup()
#head(features)

# build a sparse matrix
M <- diag(1:100) # a fully stored matrix with many zeros

dfM <- as.data.frame(as.table(M)) %>% 
  rename(row = Var1, col=Var2, value = Freq) # the same matrix stored as a data frame

sparse_M <- filter(dfM, value != 0) # a sparse represtation of M

# remove non-useful features e.g. no variance across cases
   # remove words that appear less than 18 of the reviews 0.01%
dtm <- 
  filter(features, docfreq > 18) %>% 
  cast_sparse(row=id, column=token, value = tf) 

#features %>% 
  #filter(token %in% c('about', 'what'), id %in% 1:10)
#dtm[1:10, c('about','what')]

used_rows <- as.integer(rownames(dtm))
used_amazon <- amazon[used_rows, ]
trainidx <- trainidx[used_rows]
test <- !trainidx

# Model Training!
y <- used_amazon$rating
ytest <- y[test]
ytrain<-y[trainidx]
fit_lasso_lm <- glmnet(dtm[trainidx, ], y[trainidx]) #glmnet using linear regression

# plot the lambda
#plot(fit_lasso_lm, xvar = 'lambda')

# check the predictions on the training data
pred_df <- data.frame(id = which(trainidx), rating = y[trainidx]) %>%
  mutate(pred = predict(fit_lasso_lm, dtm[trainidx, ], s = 10^-4)) # add predictions

pred_df %>%
  ggplot(aes(rating, pred)) + geom_point(position = "jitter", alpha = 0.2) +
  geom_smooth(method = "lm", col = "red")

#____________________________________________________________________________
####### codes that i added #######

# using this as precision lambda
grid <- 10^seq(10,-2,length=100) 

# alpha = 1 for LASSO
lasso.mod <- glmnet(dtm[trainidx,],y[trainidx],alpha=1,lambda=grid) 

##### Using cross-validation to choose a parameter for LASSO! #####
cv.out <- cv.glmnet(dtm[trainidx,],y[trainidx], alpha=1)
bestlam <- cv.out$lambda.min # the value that has the smallest cross-validation error
#plot(cv.out)

lasso.pred <- predict(cv.out,s = "lambda.min" ,newx = dtm[test, ])
lasso.prob <- predict(lasso.mod,s = "lambda.min" ,newx = dtm[test, ], type = "response")
View(lasso.pred)
# on our test data (NOT SURE how to get the x and y for the test data)
#lasso.coef <- predict(lasso.pred,type="coefficients",s=bestlam)[1:20,]
#lasso.all <- predict(lasso.mod,s=bestlam)

# convert to z-scores
# get the values our from the lasso result
sum_predictions_train <- sum(ytrain)
sum_predictions_test <- sum(lasso.pred)
sum_predictions <- sum(sum_predictions_train, sum_predictions_test)

sd_predictions_test <- sd(lasso.pred)*sqrt((length(lasso.pred)-1)/(length(lasso.pred)))
mean_predictions_test <- mean(lasso.pred)

z <- (5.8 - 3.0001) / sd_predictions_test
pnorm(z)
View(lasso.pred)
hist(sum_all_predictions)

# making your model for submission
pred_df = data.frame(id = which(!trainidx)) %>%
  mutate(Predict = predict(lasso.mod, newx = dtm[test, ], s = bestlam)) # add predictions

sample_submission <- read_csv("amazon_baby_testset_sample.csv")
pred_df <- sample_submission %>%
  mutate(Prediction = predict(lasso.mod, dtm[!trainidx, ], s = bestlam)[,1]) %>% # add predictions
  mutate(Prediction_pred = lasso.pred) # this is the adjusted rating that i need!
#View(pred_df)
  
# used_rows computed earlier contains all the indices of reviews used in dtm
all_test_reviews <- which(is.na(amazon$rating))
missing_test_ids <- setdiff(used_rows, all_test_reviews)

best_default_prediction <- mean(y[trainidx]) # best prediction if now review features are available
cat("best baseline prediction:", best_default_prediction,"\n")

dtm_test_predictions <- 
  data.frame(Id = as.numeric(used_rows[test]), 
             pred=predict(lasso.mod, dtm[test, ], s = bestlam)[,1])

dtm_test_predictions <- dtm_test_predictions %>%
  mutate(prob = round(pnorm((dtm_test_predictions$pred - 3.0001) / sd_predictions_test), digits = 5))
View(dtm_test_predictions)

#pred_df <- sample_submission %>%
  #left_join(dtm_test_predictions) %>%
  #mutate(Prediction = ifelse(Id %in% missing_test_ids, best_default_prediction, pred)) # add predictions

# perform a linear regression to get the probability??
#Final <- lm(pred_df$Prediction ~ dtm[test, ], data = trainidx )
#_____________________________________________________________________________

pred_df %>% 
  transmute(Id=Id, Prediction = prob) %>% 
  write_csv("my_submission.csv")
file.show("my_submission.csv")

################### (new code from Raoul) sample submission ###################

sample_submission <- read_csv("amazon_baby_testset_sample.csv")
pred_df <- sample_submission %>%
  mutate(Prediction = predict(fit_lasso_lm, dtm[!trainidx, ], s = 10^-4)[,1]) # add predictions

sample_submission = read_csv("../input/archive/amazon_baby_testset_sample.csv", col_types = cols(col_character(), col_double()))

# used_rows computed earlier contains all the indices of reviews used in dtm
all_test_reviews <- which(is.na(amazon$rating))
missing_test_ids <- setdiff(used_rows, all_test_reviews)

best_default_prediction <- mean(y[trainidx]) # best prediction if now review features are available
cat("best baseline prediction:", best_default_prediction,"\n")

dtm_test_predictions <- 
  data.frame(Id = as.character(used_rows[!trainidx]), 
             pred=predict(fit_lasso_lm, dtm[!trainidx, ], s = 10^-4)[,1]
  )

pred_df <- sample_submission %>%
  left_join(dtm_test_predictions) %>%
  mutate(Prediction = ifelse(Id %in% missing_test_ids, best_default_prediction, pred)) %>%
  mutate(prob = round(pnorm((dtm_test_predictions$pred - 3.0001) / sd_predictions_test), digits = 15))

  # add predictions




