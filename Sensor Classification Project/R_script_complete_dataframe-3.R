library(tidyverse)
library(e1071)

setwd("/home/yeachan153/Desktop/BDS/R/Big Data Analytics/Big-Data-Analytics/Sensor Classification Project/Dataset")

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
        m12 = mean(c(X1, X2)),
        m13 = mean(c(X1, X3)),
        m23 = mean(c(X2, X3)),
        m123 = mean(c(X1, X2, X3)),
        sd1 = sd(X1),
        sd2 = sd(X2),
        sd3 = sd(X3),
        var1 = var(X1),
        var2 = var(X2),
        var3 = var(X3),
        kur1 = kurtosis(X1),
        kur2 = kurtosis(X2),
        kur3 = kurtosis(X3),
        q1_25 = quantile(X1, .25),
        skew1 = e1071::skewness(X1),
        AR1.1 = cor(X1, lag(X1), use = "pairwise"),
        AR1.2 = cor(X1, lag(X1, n = 2), use = "pairwise"),
        AR12.1 = cor(X1, lag(X2), use = "pairwise"),
        # ... your own features ... (to get inspired, look at the histograms above)
        n=n()
      ) 
    
    usertimedom 
  }

# ---- read in txt files ----
act_labels  <- read_delim("activity_labels.txt", delim = " ", col_names = F, trim_ws = T) %>%
  select(X1, X2)
all_files <- unzip('Data.zip', list = T)$Name
filenames_acc <- all_files[grepl("RawData/Train/acc_exp", all_files)]
filenames_gyr <- all_files[grepl("RawData/Train/gyro_exp", all_files)]

activities <- unique(act_labels$X2)

# ---- acc data averages ----
# Data averaged -- average of the means/sd etc.
all_av_data_a <- data.frame(matrix(NA, ncol = 24, nrow = 1))
colnames(all_av_data_a) <- c("user_id", "activity", "m1", "m2", "m3", "m12", "m13", "m23", "m123", "sd1", "sd2", "sd3", "var1", "var2", "var3", "kur1", "kur2", "kur3", "q1_25", "skew1", "AR1.1", "AR1.2", "AR12.1", "n")
counter <- 1
for (j in activities) {
  for (i in 1:length(filenames_acc)) {
    all_user_data <- extractTimeDomainFeatures(filename = filenames_acc[i], labels = act_labels$X2)
    user_data_standing <- all_user_data %>% filter(activity == j)
    all_av_data_a[counter, ] <- c(i, j, user_data_standing[ ,c("m1", "m2", "m3", "m12", "m13", "m23", "m123", "sd1", "sd2", "sd3", "var1", "var2", "var3", "kur1", "kur2", "kur3", "q1_25", "skew1", "AR1.1", "AR1.2", "AR12.1", "n")] %>% apply(2, mean))
    counter <- counter + 1
  }
}

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
all_av_data_g <- data.frame(matrix(NA, ncol = 24, nrow = 1))
colnames(all_av_data_g) <- c("user_id", "activity", "m1", "m2", "m3", "m12", "m13", "m23", "m123", "sd1", "sd2", "sd3", "var1", "var2", "var3", "kur1", "kur2", "kur3", "q1_25", "skew1", "AR1.1", "AR1.2", "AR12.1", "n")
counter <- 1
for (j in activities) {
  for (i in 1:length(filenames_gyr)) {
    all_user_data <- extractTimeDomainFeatures(filename = filenames_gyr[i], labels = act_labels$X2)
    user_data_standing <- all_user_data %>% filter(activity == j)
    all_av_data_g[counter, ] <- c(i, j, user_data_standing[ ,c("m1", "m2", "m3", "m12", "m13", "m23", "m123", "sd1", "sd2", "sd3", "var1", "var2", "var3", "kur1", "kur2", "kur3", "q1_25", "skew1", "AR1.1", "AR1.2", "AR12.1", "n")] %>% apply(2, mean))
    counter <- counter + 1
  }
}

# All raw data -- dataframe with every epoch separately 
for (i in 1:length(filenames_acc)) {
  all_user_data_g <- extractTimeDomainFeatures(filename = filenames_gyr[i], labels = act_labels$X2)
  if (i == 1) {
    all_raw_data_g <- all_user_data_g
  } else {
    all_raw_data_g <- rbind(all_raw_data_g, all_user_data_g)
  }
}

path1 = "/home/yeachan153/Desktop/BDS/R/Big Data Analytics/Big-Data-Analytics/Sensor Classification Project/Dataset/all_raw_data_a_train.csv"
path2 = "/home/yeachan153/Desktop/BDS/R/Big Data Analytics/Big-Data-Analytics/Sensor Classification Project/Dataset/all_raw_data_g_train.csv"

write.csv(all_raw_data_a, file = path1, row.names = F, fileEncoding = "UTF-8")
write.csv(all_raw_data_g, file = path2, row.names = F, fileEncoding = "UTF-8")
