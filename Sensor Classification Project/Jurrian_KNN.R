rm(list=ls())
library(tidyverse)
library(e1071)

setwd("D:/Documenten/BDS/Big Data Analytics/Competition 2")

### ---- FUN extract Time Domain Features -----
extractTimeDomainFeatures <- 
  function(filename, labels = train_labels) {
    # extract user and experimental run ID's from file name
    username = gsub(".+user(\\d+).+", "\\1", filename)
    expname =  gsub(".+exp(\\d+).+", "\\1", filename)
    
    # import the data from the file
    user01 <- read_delim(filename, " ", col_names = F, progress = TRUE, col_types = "ddd")
    
    # creating labels
    labels  <- suppressMessages(read_delim("RawData/Train/labels_train.txt", delim = " ", col_names = F))
    colnames(labels)  <- c("trial", "userid", "activity", "start", "end")
    labels  <- labels %>% 
      mutate(activity = act_labels$X2[activity])
    
    # select this users activity labels from the `labels` frame for this experimental run
    user_labels <- 
      labels %>% 
      dplyr::filter(userid == as.numeric(username) & trial == as.numeric(expname)) %>% # select rows pertaining to current signals
      mutate(segment = row_number()) %>%                                             # segment identifies different WALKING_UPSTAIRS etc
      gather(start_end, vec, -trial, -userid, -activity, -segment) %>%               # stack start and end on top of each other
      arrange(vec) %>%                                                               # arrange rows in natural order
      mutate(activity = ifelse(start_end == 'end', NA, activity), activity_id = row_number()) # remove activity label `end` time rows
    
    # add activity labels to each sample
    user <- 
      user01 %>% 
      mutate(sample = row_number()-1) %>%
      mutate(activity_id = findInterval(sample, user_labels$vec)) %>%
      left_join(user_labels, by = "activity_id") 
    
    # split in epochs of 128 samples and compute features per epoch
    usertimedom <- 
      user %>%
      mutate(epoch = sample %/% 128) %>% # epoch = 2.56 sec
      group_by(epoch) %>%
      summarise(
        user_id = username, # added to identify user in data frame rows
        exp_id = expname,   # added to identify experimental run in data frame rows
        activity = names(which.max(table(c("-", activity)))),
        sample = sample[1],
        m1 = mean(X1), 
        m2 = mean(X2),
        m3 = mean(X3),
        sd1 = sd(X1),
        sd2 = sd(X2),
        sd3 = sd(X3),
        var1 = var(X1),
        var2 = var(X2),
        var3 = var(X3),
        q1_25 = quantile(X1, .25),
        q1_75 = quantile(X1, .75),
        q2_25 = quantile(X2, .25),
        q2_75 = quantile(X2, .75),
        q3_25 = quantile(X3, .25),
        q3_75 = quantile(X3, .75),
        #skew1 = e1071::skewness(X1),
        #skew2 = e1071::skewness(X2),
        #skew3 = e1071::skewness(X3),
        AR1.1 = cor(X1, lag(X1), use = "pairwise"),
        AR1.2 = cor(X1, lag(X1, n = 2), use = "pairwise"),
        AR12.1 = cor(X1, lag(X2), use = "pairwise"),
        eucm = mean(sqrt(X1^2 + X2^2 + X3^2)),
        rng1 = abs(max(X1)-min(X1)),
        rng2 = abs(max(X2)-min(X2)),
        rng3 = abs(max(X3)-min(X3)),
        ml1 = unname(sort(table(round(X1,1)),decreasing = T))[1],
        ml2 = unname(sort(table(round(X2,1)),decreasing = T))[1],
        ml3 = unname(sort(table(round(X3,1)),decreasing = T))[1],
        am1 = mean(abs(X1)),
        am2 = mean(abs(X2)),
        am3 = mean(abs(X3)),
        #fmed1 = sum(round(X1,1) == round(median(X1),1)),
        #fmed2 = sum(round(X2,1) == round(median(X2),1)),
        #fmed3 = sum(round(X3,1) == round(median(X3),1)),
        # ... your own features ... (to get inspired, look at the histograms above)
        n=n()
      ) 
    
    usertimedom 
  }

# ---- read in txt files ----
act_labels  <- read_delim("activity_labels.txt", delim = " ", col_names = F, trim_ws = T) %>%
  select(X1, X2)
all_files <- unzip('all.zip', list = T)$Name
filenames_acc <- all_files[grepl("RawData/Train/acc_exp", all_files)]
filenames_gyr <- all_files[grepl("RawData/Train/gyro_exp", all_files)]

activities <- unique(act_labels$X2)
#activities <- c(activities, "NA")
# ---- acc data averages ----
# Data averaged -- average of the means/sd etc.
#all_av_data_a <- data.frame(matrix(NA, ncol = 24, nrow = 1))
#colnames(all_av_data_a) <- c("user_id", "activity", "m1", "m2", "m3", "m12", "m13", "m23", "m123", "sd1", "sd2", "sd3", "var1", "var2", "var3", "kur1", "kur2", "kur3", "q1_25", "skew1", "AR1.1", "AR1.2", "AR12.1", "n")
#counter <- 1
#for (j in activities) {
#   for (i in 1:length(filenames_acc)) {
#     all_user_data <- extractTimeDomainFeatures(filename = filenames_acc[i], labels = act_labels$X2)
#     user_data_standing <- all_user_data %>% filter(activity == j)
#     all_av_data_a[counter, ] <- c(i, j, user_data_standing[ ,c("m1", "m2", "m3", "m12", "m13", "m23", "m123", "sd1", "sd2", "sd3", "var1", "var2", "var3", "kur1", "kur2", "kur3", "q1_25", "skew1", "AR1.1", "AR1.2", "AR12.1", "n")] %>% apply(2, mean))
#     counter <- counter + 1
#   }
# }

# All raw data -- dataframe with every epoch separately 
for (i in 1:length(filenames_acc)) {
  all_user_data_a <- extractTimeDomainFeatures(filename = filenames_acc[i], labels = act_labels$X2)
  if (i == 1) {
    all_raw_data_a <- all_user_data_a
  } else {
    all_raw_data_a <- rbind(all_raw_data_a, all_user_data_a)
  }
}



# ---- gyro data averages ----
# Data averaged -- average of the means/sd etc. 
# all_av_data_g <- data.frame(matrix(NA, ncol = 24, nrow = 1))
# colnames(all_av_data_g) <- c("user_id", "activity", "m1", "m2", "m3", "m12", "m13", "m23", "m123", "sd1", "sd2", "sd3", "var1", "var2", "var3", "kur1", "kur2", "kur3", "q1_25", "skew1", "AR1.1", "AR1.2", "AR12.1", "n")
# counter <- 1
# for (j in activities) {
#   for (i in 1:length(filenames_gyr)) {
#     all_user_data <- extractTimeDomainFeatures(filename = filenames_gyr[i], labels = act_labels$X2)
#     user_data_standing <- all_user_data %>% filter(activity == j)
#     all_av_data_g[counter, ] <- c(i, j, user_data_standing[ ,c("m1", "m2", "m3", "m12", "m13", "m23", "m123", "sd1", "sd2", "sd3", "var1", "var2", "var3", "kur1", "kur2", "kur3", "q1_25", "skew1", "AR1.1", "AR1.2", "AR12.1", "n")] %>% apply(2, mean))
#     counter <- counter + 1
#   }
# }

# All raw data -- dataframe with every epoch separately 
for (i in 1:length(filenames_acc)) {
  all_user_data_g <- extractTimeDomainFeatures(filename = filenames_gyr[i], labels = act_labels$X2)
  if (i == 1) {
    all_raw_data_g <- all_user_data_g
  } else {
    all_raw_data_g <- rbind(all_raw_data_g, all_user_data_g)
  }
}

################################################################

#merge raw train dataframes

all_raw_data = merge(all_raw_data_a,all_raw_data_g,
                     by = c("user_id","exp_id","epoch"))
all_raw_data = all_raw_data[order(all_raw_data$user_id,
                                  all_raw_data$exp_id,
                                  as.numeric(all_raw_data$epoch)),]
all_raw_data = all_raw_data[,-which(names(all_raw_data) %in% c("activity.y",
                                                               "n.y",
                                                               "n.x",
                                                               "sample.x",
                                                               "sample.y"))]
all_raw_data = all_raw_data[,4:ncol(all_raw_data)]

all_raw_data$activity.x = factor(all_raw_data$activity.x)

#boxplots and manova
for(i in 2:ncol(all_raw_data)){
  boxplot(all_raw_data[,i]~all_raw_data$activity.x, main = names(all_raw_data)[i])
}

man1 = manova(as.matrix(all_raw_data[,2:ncol(all_raw_data)]) ~ all_raw_data$activity.x)
saov = summary.aov(man1)

fvalues = c()
for(i in 1:(ncol(all_raw_data)-1)){
  fvalues[i] = sqrt(saov[[i]]$`F value`[1])
}

plot(fvalues,col = "white")
text(fvalues,names(all_raw_data[,2:ncol(all_raw_data)]))
abline(h = 20,col = "red")

fv15 = names(all_raw_data)[which(fvalues > 15)+1]

all_raw_data = all_raw_data[,c("activity.x",fv15)]



all_raw_data = all_raw_data[,c("activity.x",fv20)]
rownames(all_raw_data) = 1:nrow(all_raw_data)
all_raw_data$AR1.1.x[is.na(all_raw_data$AR1.1.x)] = mean(all_raw_data$AR1.1.x[all_raw_data$activity.x == "-"],na.rm = T)
all_raw_data$AR1.2.x[is.na(all_raw_data$AR1.2.x)] = mean(all_raw_data$AR1.2.x[all_raw_data$activity.x == "-"],na.rm = T)

library("rknn")

# kacc = data.frame(base::matrix(NA,ncol = 0,nrow = 10))
# for(i in 5:15){
#   beg = rknnBeg(as.matrix(all_raw_data[,2:ncol(all_raw_data)]),
#                 all_raw_data[,1], 
#                 k = i, 
#                 r = 500)
#   bs = bestset(beg,criterion = "mean_accuracy")
#   trctrl = trainControl(method = "repeatedcv", number = 10, repeats = 3)
#   set.seed(3333)
#   knn_fit = train(activity.x ~., data = all_raw_data[,c("activity.x",bs)], method = "knn",
#                   trControl=trctrl,
#                   preProcess = c("center", "scale"),
#                   tuneLength = 10)
#   a = cbind(knn_fit$results$k,knn_fit$results$Accuracy)
#   kacc = cbind(kacc,a)
#   print(kacc)
# }


beg = rknnBeg(as.matrix(all_raw_data[,2:ncol(all_raw_data)]),
              all_raw_data[,1], 
              k = 10, 
              r = 500)
bs = bestset(beg,criterion = "mean_accuracy")

#fit model

library(caret)

trctrl = trainControl(method = "repeatedcv", number = 10, repeats = 3)
set.seed(3333)
knn_fit = train(activity.x ~., data = all_raw_data[,c("activity.x",bs)], method = "knn",
                trControl=trctrl,
                preProcess = c("center", "scale"),
                tuneLength = 10)
knn_fit
#create separate acc and gyro dataframes for test data

all_files <- unzip('all.zip', list = T)$Name
filenames_acc_t <- all_files[grepl("RawData/Test/acc_exp", all_files)]
filenames_gyr_t <- all_files[grepl("RawData/Test/gyro_exp", all_files)]

for (i in 1:length(filenames_acc_t)) {
  all_user_data_at <- extractTimeDomainFeatures(filename = filenames_acc_t[i], labels = act_labels$X2)
  if (i == 1) {
    all_raw_data_at <- all_user_data_at
  } else {
    all_raw_data_at <- rbind(all_raw_data_at, all_user_data_at)
  }
}
for (i in 1:length(filenames_acc_t)) {
  all_user_data_gt <- extractTimeDomainFeatures(filename = filenames_gyr_t[i], labels = act_labels$X2)
  if (i == 1) {
    all_raw_data_gt <- all_user_data_gt
  } else {
    all_raw_data_gt <- rbind(all_raw_data_gt, all_user_data_gt)
  }
}

#merge raw test dataframes

testdata = merge(all_raw_data_at,all_raw_data_gt,
                 by = c("user_id","exp_id","epoch"))
testdata = testdata[order(testdata$user_id,
                          testdata$exp_id,
                          as.numeric(testdata$epoch)),]
testdata = testdata[,-which(names(testdata) %in% c("activity.y",
                                                   "n.y",
                                                   "n.x",
                                                   "sample.y"))]
testdata$activity.x = rep(NA,nrow(testdata))

#predictions

test_pred = predict(knn_fit, newdata = testdata)
test_pred

testdata$activity.x = test_pred

colnames(testdata)[which(names(testdata)=="activity.x")] = "activity"
colnames(testdata)[which(names(testdata)=="sample.x")] = "sample"

testdata1 = testdata

testdata1 %>%
  mutate(user_id = paste("user", user_id, sep=""), exp_id = paste("exp", exp_id, sep="")) %>%
  unite(Id, user_id, exp_id, sample) %>%
  select(Id, Predicted = activity) %>%
  write_csv("test_set_predictions1.csv")
