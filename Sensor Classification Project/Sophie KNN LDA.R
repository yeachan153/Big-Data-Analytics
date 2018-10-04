wdSophie <- "C:/Users/Sophie/Desktop/Psychologie M1/Big Data Analysis/Assignments/2. Physical activity recognition"
setwd(wdSophie)
save.image("ldatryout.V2.Rdata")
save.image("scriptModelComparisonComplete.Rdata")
load("scriptModelComparisonComplete.Rdata") #results from this script

###load libraries and data
    load("scriptComplete210.Rdata") #result from total script 1.R sent by Jur
    library(MASS)
    library(rknn)
    library(klaR)
    library(tidyverse)

#####FEATURE SELECTION#####
############################################################################

###Function to create new variables from original variables (interactions, transformations)
    newvars <- function(allrawdata,varsexcluded){
        
      output <- allrawdata %>%
      #select(-17,-18,-19,-24,-25,-26,-46,-47,-48,-53,-54,-55) %>%
      #select(-21,-22,-23,-28,-29,-30,-52,-53,-54,-59,-60,-61) %>%
      select(varsexcluded) %>%
        #due to some problems with the variables:
        #-24,-25,-26,-53,-54,-55 (the ml-features) for fitting lda
        #-17,-18,-19,-46,-47,-48 (the AR-features) in backward selection; due to multicollinearity according to Peter
      
      #two-way interactions .x * .y
      mutate(m1.int=m1.x*m1.y) %>% #add interactions, e.g. interaction x and y features
      mutate(m2.int=m2.x*m2.y) %>%
      mutate(m3.int=m3.x*m3.y) %>%
      mutate(sd1.int=sd1.x*sd1.y) %>%
      mutate(sd2.int=sd2.x*sd2.y) %>%
      mutate(sd3.int=sd3.x*sd3.y) %>%
      mutate(var1.int=var1.x*var1.y) %>%
      mutate(var2.int=var2.x*var2.y) %>%
      mutate(var3.int=var3.x*var3.y) %>%
      mutate(q1_25.int=q1_25.x*q1_25.y) %>%
      mutate(q2_25.int=q2_25.x*q2_25.y) %>%
      mutate(q3_25.int=q3_25.x*q3_25.y) %>%
      mutate(q1_75.int=q1_75.x*q1_75.y) %>%
      mutate(q2_75.int=q2_75.x*q2_75.y) %>%
      mutate(q3_75.int=q3_75.x*q3_75.y) %>%    
      mutate(eucm.int=eucm.x*eucm.y) %>%
      mutate(am1.int=am1.x*am1.y) %>%
      mutate(am2.int=am2.x*am2.y) %>%
      mutate(am3.int=am3.x*am3.y) %>%
      #mutate(fmed1.int=fmed1.x*fmed1.y) %>% #%>% fmed2 and 3 still missing
      #mutate(fmed2.int=fmed2.x*fmed2.y) %>%
      #mutate(fmed3.int=fmed3.x*fmed3.y) %>%
        
      #three-way interactions x1*x2*x3
      mutate(m.x.int=m1.x*m2.x*m3.x) %>%
      mutate(sd.x.int=sd1.x*sd2.x*sd3.x) %>%
      mutate(var.x.int=var1.x*var2.x*var3.x) %>%
      mutate(q_25.x.int=q1_25.x*q2_25.x*q3_25.x) %>%
      mutate(q_75.x.int=q1_75.x*q2_75.x*q3_75.x) %>% 
      mutate(am.x.int=am1.x*am2.x*am3.x) %>%
      #mutate(fmed.x.int=fmed1.x*fmed2.x*fmed3.x) %>%
      mutate(m.y.int=m1.y*m2.y*m3.y) %>%
      mutate(sd.y.int=sd1.y*sd2.y*sd3.y) %>%
      mutate(var.y.int=var1.y*var2.y*var3.y) %>%
      mutate(q_25.y.int=q1_25.y*q2_25.y*q3_25.y) %>%
      mutate(q_75.y.int=q1_75.y*q2_75.y*q3_75.y) %>% 
      mutate(am.y.int=am1.y*am2.y*am3.y) %>%
        
      #linear transformations
      mutate(m1.x.tr2=m1.x^2) %>%
      mutate(m2.x.tr2=m2.x^2) %>%
      mutate(m3.x.tr2=m3.x^2) %>%
      mutate(sd1.x.tr2=sd1.x^2) %>%
      mutate(sd2.x.tr2=sd2.x^2) %>%
      mutate(sd3.x.tr2=sd3.x^2) %>%
      mutate(var1.x.tr2=var1.x^2) %>%
      mutate(var2.x.tr2=var2.x^2) %>%
      mutate(var3.x.tr2=var3.x^2) %>%
      mutate(q1_25.x.tr2=q1_25.x^2) %>%
      mutate(q2_25.x.tr2=q2_25.x^2) %>%
      mutate(q3_25.x.tr2=q3_25.x^2) %>%
      mutate(q1_75.x.tr2=q1_75.x^2) %>%
      mutate(q2_75.x.tr2=q2_75.x^2) %>%
      mutate(q3_75.x.tr2=q3_75.x^2) %>%
      mutate(eucm.x.tr2=eucm.x^2) %>%
      mutate(rng1.x.tr2=rng1.x^2) %>%
      mutate(rng2.x.tr2=rng2.x^2) %>%
      mutate(rng3.x.tr2=rng3.x^2) %>%
      mutate(am1.x.tr2=am1.x^2) %>%
      mutate(am2.x.tr2=am2.x^2) %>%
      mutate(am3.x.tr2=am3.x^2) %>%
      #mutate(fmed1.x.tr2=fmed1.x^2) %>%
      mutate(m1.y.tr2=m1.y^2) %>%
      mutate(m2.y.tr2=m2.y^2) %>%
      mutate(m3.y.tr2=m3.y^2) %>%
      mutate(sd1.y.tr2=sd1.y^2) %>%
      mutate(sd2.y.tr2=sd2.y^2) %>%
      mutate(sd3.y.tr2=sd3.y^2) %>%
      mutate(var1.y.tr2=var1.y^2) %>%
      mutate(var2.y.tr2=var2.y^2) %>%
      mutate(var3.y.tr2=var3.y^2) %>%
      mutate(q1_25.y.tr2=q1_25.y^2) %>%
      mutate(q2_25.y.tr2=q2_25.y^2) %>%
      mutate(q3_25.y.tr2=q3_25.y^2) %>%
      mutate(q1_75.y.tr2=q1_75.y^2) %>%
      mutate(q2_75.y.tr2=q2_75.y^2) %>%
      mutate(q3_75.y.tr2=q3_75.y^2) %>%
      mutate(eucm.y.tr2=eucm.y^2) %>%
      mutate(rng1.y.tr2=rng1.y^2) %>%
      mutate(rng2.y.tr2=rng2.y^2) %>%
      mutate(rng3.y.tr2=rng3.y^2) %>%
      mutate(am1.y.tr2=am1.y^2) %>%
      mutate(am2.y.tr2=am2.y^2) %>%
      mutate(am3.y.tr2=am3.y^2) 
      
      return(output)
      }
    tryout <- newvars(all_raw_data, c(-17,-18,-19,-24,-25,-26,-45,-46,-47,-52,-53,-54))
    #tryout <- newvars(all_raw_data_original, c(-21,-22,-23,-28,-29,-30,-52,-53,-54,-59,-60,-61))
    #View(head(tryout)) #128 columns
    testdata_new <- newvars(testdata,c(-21,-22,-23,-28,-29,-30,-50,-51,-52,-57,-58,-59,-34,-63)) #exclude AR,ml and fmed vars
    
###LDA with backward selection
    #model without interactions
    lda.backwardselection <- stepclass(tryout,grouping=tryout$activity.x, method='lda', direction='backward') 
    lda.backwardselection$model #model recommended by backward selection (40 vars)
    lda.backwardselection$result.pm #correctness rate: 0.77975
    #min.elapsed: 15.00
    
    #model with interactions gyro(x) and acc(y)
    lda.backwardselection.V2 <- stepclass(tryout,grouping=tryout$activity.x, method='lda', direction='backward') 
    lda.backwardselection.V2$model #model recommended by backward selection (57 vars)
    lda.backwardselection.V2$result.pm #correctness rate: 0.7848682
    #min.elapsed: 33.00
    
    #model with interactions x1*x2*x3
    tryoutV3 <- tryout %>% select(c(1,lda.backwardselection.V2$model$nr,68:ncol(tryout))) 
      #select new features+selected features from previous model (69 predicter vars in total)
    lda.backwardselection.V3 <- stepclass(tryoutV3,grouping=tryoutV3$activity.x, method='lda', direction='backward') 
    lda.backwardselection.V3$model #model recommended by backward selection (60 vars)
    lda.backwardselection.V3$result.pm #correctness rate: 0.7882247
    #min.elapsed: 30.00
    
    #model with transformations
    tryoutV4 <- tryout %>% select(c(1,lda.backwardselection.V3$model$nr,80:ncol(tryout))) 
    #select new features+selected features from previous model (107 predicter vars in total)
    lda.backwardselection.V4 <- stepclass(tryoutV4,grouping=tryoutV4$activity.x, method='lda', direction='backward', fold=5) 
        #5-fold instead of 10
    lda.backwardselection.V4$model #model recommended by backward selection (99 vars)
    lda.backwardselection.V4$result.pm #correctness rate: 0.7997475
    #min.elapsed: 36:00
    
    #backforwardselection with less strict stopping rule and 2-fold cross validation
    tryoutV5 <- tryout %>% select(c(1,lda.backwardselection.V4$model$nr)) 
    lda.backwardselection.V5 <- stepclass(tryoutV5,grouping=tryoutV5$activity.x, method='lda', direction='backward', improvement=.01, fold=2) 
    lda.backwardselection.V5$model #model recommended by backward selection (91 vars)
    lda.backwardselection.V5$result.pm #correctness rate: 0.7898273
    #min.elapsed: 9:00
    
    #backforwardselection and forward-selection with previous selected variables (errors)
      #lda.backforwardselection.V5 <- stepclass(tryoutV5,grouping=tryoutV5$activity.x, method='lda', direction='both', fold=5) 
      #lda.forwardselection.V5 <- stepclass(tryoutV5,grouping=tryoutV5$activity.x, method='lda', direction='forward', fold=5) 
    
    tryoutV6 <- tryout %>% select(c(1,lda.backwardselection.V5$model$nr)) 
    #91 variables instead of 125


###KNN random selection
    #bs.V1 calculated with k=10 contains 16 vars
    #bs.V2 calculated with K=1 contains 31 vars
    #bs.V3 calculated with k=20 contains 16 vars (different vars than b1.v1)
    #bs.V4 calculated with k=10 after backward selection(v4) contains 25 vars
    #bs.V5 calculated with K=5 contains 31 vars
    #model v6 only uses backward selection, no rKNN, contains 97 vars
    #bs.V7 calculated with K=20 after backward selection(v4) contains 48 vars
    
    #beg.V7 = rknnBeg(as.matrix(tryout[,c(1,lda.backwardselection.V4$model$nr[c(-19,-39)])][,-1]),
    #                 tryout[,c(1,lda.backwardselection.V4$model$nr[c(-19,-39)])][,1], 
    #                 k = 20, 
    #                 r = 500)
    #bs.V7 = bestset(beg.V7,criterion = "mean_accuracy")
    

    
#####MODEL FITTING#####
############################################################################
###fit knn
    library(caret)
    trctrl = trainControl(method = "repeatedcv", number = 10, repeats = 3)
    
    #Model 1 feature selection: KNN (K=10) with all vars; model contains 16 vars
    set.seed(3333)
      knn_fit.bsV1 <- train(activity.x ~., data = tryout[,c("activity.x",bs.V1)], method = "knn", trControl=trctrl, preProcess = c("center", "scale"), tuneLength = 10)
      lda_fit.bsv1 <- lda(activity.x ~., data = tryout[,c("activity.x",bs.V1)])
      qda_fit.bsv1 <- qda(activity.x ~., data = tryout[,c("activity.x",bs.V1)])
    #Model 2 feature selection: KNN (K=1) with all vars; model contains 31 vars
    set.seed(3333)
      knn_fit.bsV2 <- train(activity.x ~., data = tryout[,c("activity.x",bs.V2)], method = "knn", trControl=trctrl, preProcess = c("center", "scale"), tuneLength = 10)
      lda_fit.bsv2 <- lda(activity.x ~., data = tryout[,c("activity.x",bs.V2)])
      qda_fit.bsv2 <- qda(activity.x ~., data = tryout[,c("activity.x",bs.V2)])
    #Model 3 feature selection: KNN (K=20) with all vars; model contains 16 vars
    set.seed(3333)
      knn_fit.bsV3 <- train(activity.x ~., data = tryout[,c("activity.x",bs.V3)], method = "knn", trControl=trctrl, preProcess = c("center", "scale"), tuneLength = 10)
      lda_fit.bsv3 <- lda(activity.x ~., data = tryout[,c("activity.x",bs.V3)])
      qda_fit.bsv3 <- qda(activity.x ~., data = tryout[,c("activity.x",bs.V3)])
    #Model 4 feature selection: KNN (K=10) after backward-selection vars (V4) with all vars; model contains 25 vars
    set.seed(3333)
      knn_fit.bsV4 <- train(activity.x ~., data = tryout[,c("activity.x",bs.V4)], method = "knn", trControl=trctrl, preProcess = c("center", "scale"), tuneLength = 10)
      lda_fit.bsv4 <- lda(activity.x ~., data = tryout[,c("activity.x",bs.V4)])
      qda_fit.bsv4 <- qda(activity.x ~., data = tryout[,c("activity.x",bs.V4)])
    #Model 5 feature selection: KNN (K=5) with all vars; model contains 31 vars
    set.seed(3333)
      knn_fit.bsV5 <- train(activity.x ~., data = tryout[,c("activity.x",bs.V5)], method = "knn", trControl=trctrl, preProcess = c("center", "scale"), tuneLength = 10)
      lda_fit.bsv5 <- lda(activity.x ~., data = tryout[,c("activity.x",bs.V5)]) #colinearity!
      qda_fit.bsv5 <- qda(activity.x ~., data = tryout[,c("activity.x",bs.V5)]) #rank deficiency in group!
    #Model 6 feature selection: backward-selection (V4) with all vars; model contains 97 vars (fmed.x and fmed.y excluded)
    set.seed(3333) 
      knn_fit.bsV6 <- train(activity.x ~., data = tryout[,c(1,lda.backwardselection.V4$model$nr[c(-19,-39)])], method = "knn", trControl=trctrl, preProcess = c("center", "scale"), tuneLength = 10)
      lda_fit.bsv6 <- lda(activity.x ~., data = tryout[,c(1,lda.backwardselection.V4$model$nr[c(-19,-39)])]) #colinearity!
      qda_fit.bsv6 <- qda(activity.x ~., data = tryout[,c(1,lda.backwardselection.V4$model$nr[c(-19,-39)])]) #some group too small for qda
      
    #Model 7 feature selection: KNN (K=20) after backward-selection vars (V4) with all vars; model contains 25 vars
    set.seed(3333) 
      knn_fit.bsV7 <- train(activity.x ~., data = tryout[,c("activity.x",bs.V7)], method = "knn", trControl=trctrl, preProcess = c("center", "scale"), tuneLength = 10)
      lda_fit.bsv7 <- lda(activity.x ~., data = tryout[,c("activity.x",bs.V7)]) #colinearity!
      qda_fit.bsv7 <- qda(activity.x ~., data = tryout[,c("activity.x",bs.V7)]) #some group too small for qda!
      
      
#####PREDICTIONS##### 
############################################################################
#predictions and prediction-rate bootstrap (% correctly predicted 1/5 of random sample of train-dataset; repeated 50 times):
rm(testnums)      
testPrediction <- function(testdataset,traindataset,knn_fit,lda_fit,qda_fit){
  
  test_pred_knn <- predict(knn_fit, newdata = testdataset)
  test_pred_lda <- predict(lda_fit, newdata = testdataset)
  test_pred_qda <- predict(qda_fit, newdata = testdataset)
  
  set.seed(3333)
  rate_knn <- numeric()
  rate_lda <- numeric()
  rate_qda <- numeric()
  testnums <- numeric()
  i <- 0
  
  for(i in 1:50){
    testnums <- sample(1:nrow(traindataset),nrow(traindataset)/5,replace=T)
    
    pred_knn_fit <- predict(knn_fit, newdata = traindataset[testnums,])
    rate_knn <- c(rate_knn,sum(pred_knn_fit==traindataset$activity.x[testnums]))
    
    pred_lda_fit <- predict(lda_fit, traindataset[testnums,])    
    rate_lda <- c(rate_lda,sum(pred_lda_fit$class==traindataset$activity.x[testnums]))
    
    pred_qda_fit <- predict(qda_fit,traindataset[testnums,])    
    rate_qda <- c(rate_qda,sum(pred_qda_fit$class==traindataset$activity.x[testnums]))
  }
  
  rate_knn <- round(mean(rate_knn/length(testnums))*100,2)
  rate_lda <- round(mean(rate_lda/length(testnums))*100,2)
  rate_qda <- round(mean(rate_qda/length(testnums))*100,2)
  
  testPredictions <- list(KNN=test_pred_knn,LDA=test_pred_lda,QDA=test_pred_qda)
  rates <- list(KNN=rate_knn,LDA=rate_lda,QDA=rate_qda)
  return(list(Predictions=testPredictions,Rates=rates))
}
model1 <- testPrediction(testdata_new,tryout,knn_fit.bsV1,lda_fit.bsv1,qda_fit.bsv1)
model2 <- testPrediction(testdata_new,tryout,knn_fit.bsV2,lda_fit.bsv2,qda_fit.bsv2)
model3 <- testPrediction(testdata_new,tryout,knn_fit.bsV3,lda_fit.bsv3,qda_fit.bsv3)
model4 <- testPrediction(testdata_new,tryout,knn_fit.bsV4,lda_fit.bsv4,qda_fit.bsv4)
model5 <- testPrediction(testdata_new,tryout,knn_fit.bsV5,lda_fit.bsv5,qda_fit.bsv4)
model6 <- testPrediction(testdata_new,tryout,knn_fit.bsV6,lda_fit.bsv6,qda_fit.bsv4)
model7 <- testPrediction(testdata_new,tryout,knn_fit.bsV7,lda_fit.bsv7,qda_fit.bsv4)

#which model is best?
bootstrapPredictions <- as.data.frame(rbind(
      unlist(as.numeric(model1$Rates)),
      unlist(as.numeric(model2$Rates)),
      unlist(as.numeric(model3$Rates)),
      unlist(as.numeric(model4$Rates)),
      unlist(as.numeric(model5$Rates)),
      unlist(as.numeric(model6$Rates)),
      unlist(as.numeric(model7$Rates))))
bootstrapPredictions$nFeatures <- c(length(bs.V1),length(bs.V2),length(bs.V3),length(bs.V4),length(bs.V5),length(lda.backwardselection.V4$model$nr[c(-19,-39)]),length(bs.V7))
colnames(bootstrapPredictions) <- c("KNN","LDA",'QDA','nVars')
bootstrapPredictions[5,3] <- NA #model not predicted due to errors
bootstrapPredictions[6,3] <- NA
bootstrapPredictions[7,3] <- NA
bootstrapPredictions[,-3]
#best model: KNN-model4 - fewest variables while highest prediction accuracy


#Save in csv-file format
testdata_new$activity.x = model4$Predictions$KNN
colnames(testdata_new)[which(names(testdata_new)=="activity.x")] = "activity"
colnames(testdata_new)[which(names(testdata_new)=="sample.x")] = "sample"

Kaggle0310.V1 <- testdata_new %>% 
  mutate(user_id = paste("user", user_id, sep=""), exp_id = paste("exp", exp_id, sep="")) %>%
  unite(Id, user_id, exp_id, sample) %>%
  select(Id, Predicted = activity) %>%
  write_csv("Kaggle0310.V1.csv")
