load("physicalActivityRecognition.V1.RData")

library(tidyverse)
library(e1071)

wdThomas <- "/Users/Thomas/Documents/Behavioural_Data_Science/Big Data Analytics/Assignment 2/"
wdSophie <- "C:/Users/Sophie/Desktop/Psychologie M1/Big Data Analysis/Assignments/2. Physical activity recognition"
setwd(wdSophie)

act_labels

#Add dummy variables
data_withdummies <- 
  all_raw_data %>%
  mutate(dummy_walking = ifelse(activity=="WALKING",1,0)) %>%
  mutate(dummy_walking_upstairs = ifelse(activity=="WALKING_UPSTAIRS",1,0)) %>%
  mutate(dummy_walking_downstairs = ifelse(activity=="WALKING_DOWNSTAIRS",1,0)) %>%
#var 28(walking), 29(walking_upstairs), 30(walking_downstairs)

#Correlations per activity (find best predictors?)
pred.walking <- all_raw_data %>%
  filter(activity=="WALKING") %>%
  select(6:26)
cor.walking <- cor(pred.walking)

cor.walking_upstairs <- all_raw_data %>%
  filter(activity=="WALKING_UPSTAIRS") %>%
  select(6:26) %>%
  cor()

cor.walking_downstairs <- all_raw_data %>%
  filter(activity=="WALKING_DOWNSTAIRS") %>%
  select(6:26) %>%
  cor()
#etc.
colnames(data_withdummies)

###Logistic regression
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


###
