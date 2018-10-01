load("physicalActivityRecognition.V1.RData") #outcome from Thomas' code

library(tidyverse)
library(e1071)
wdThomas <- "/Users/Thomas/Documents/Behavioural_Data_Science/Big Data Analytics/Assignment 2/"
wdSophie <- "C:/Users/Sophie/Desktop/Psychologie M1/Big Data Analysis/Assignments/2. Physical activity recognition"
setwd(wdSophie)


###Add dummy variables and test-selection
randomtestselection <- sample(1:nrow(all_raw_data),size=nrow(all_raw_data)/10) #select 10% of the observations as test-data
data_withdummies <- 
  all_raw_data %>%
  mutate(observationNum = 1:6252) %>%
  mutate(dummy_walking = ifelse(activity=="WALKING",1,0)) %>%
  mutate(dummy_walking_upstairs = ifelse(activity=="WALKING_UPSTAIRS",1,0)) %>%
  mutate(dummy_walking_downstairs = ifelse(activity=="WALKING_DOWNSTAIRS",1,0)) %>%
  mutate(traindata = ifelse(data_withdummies$observationNum %in% randomtestselection,0,1))
#var 28(walking), 29(walking_upstairs), 30(walking_downstairs), 31(traindata - yes(1) or no(0))


###Correlations per activity (to find best predictors?)
cor.walking <- all_raw_data %>%
  filter(activity=="WALKING") %>%
  select(6:26) %>%
  cor()

cor.walking_upstairs <- all_raw_data %>%
  filter(activity=="WALKING_UPSTAIRS") %>%
  select(6:26) %>%
  cor()

cor.walking_downstairs <- all_raw_data %>%
  filter(activity=="WALKING_DOWNSTAIRS") %>%
  select(6:26) %>%
  cor()
  #etc.


###Logistic regression
#glmfit_full_walking <- glm (dummy_walking ~ m1+m2+m3+m12+m13+m23+m123+sd1+sd2+sd3+var1+var2+var3+kur1+kur2+kur3+q1_25+skew1,
#                 data=data_withdummies , family = binomial, subset=data_withdummies$traindata) #add AR1.1, AR1.2, AR12.1?
  #problem: including subset = data_withdummies$traindata > model does not converge anymore?
  #so for now just include full dataset:
glmfit_full_walking <- glm (dummy_walking ~ m1+m2+m3+m12+m13+m23+m123+sd1+sd2+sd3+var1+var2+var3+kur1+kur2+kur3+q1_25+skew1,
                            data=data_withdummies , family = binomial) #add AR1.1, AR1.2, AR12.1?
summary(glmfit_full_walking) #repeat for other activities
summary (glmfit_full_walking)$coef [,4]
probs_walking <- round(predict(glmfit_full_walking,type="response"),3) #probability that an activity is 'walking'
#select variables with highest correlations instead of full model?

  #summary table
  glmprediction_walking <- rep("OtherActivity",length(probs_walking))
  glmprediction_walking[probs_walking>.5] <- "Walking"
  trueclassification_walking <- ifelse(data_withdummies$dummy_walking==1,"Walking","OtherActivity")
  table(glmprediction_walking,trueclassification_walking)
  mean(glmprediction_walking == trueclassification_walking) #how many observations were correctly or incorrectly classified.
  #predict over training data

  
  
###...Other models (LDA,QDA,KNN etc.)
