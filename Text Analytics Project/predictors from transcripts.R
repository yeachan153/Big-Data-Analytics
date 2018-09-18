# Make sure to change this
setwd("/home/yeachan153/Desktop/BDS/R/Big Data Analytics/Big-Data-Analytics/Text Analytics Project")
library(tidyverse)

" 
Read in the CSV file
"
personality = as.tibble(read.csv("youtube-personality/YouTube-Personality-Personality_impression_scores_train.csv", 
                                 encoding = "UTF-8",
                                 sep = " "))

gender = as.tibble(read.csv("youtube-personality/YouTube-Personality-gender.csv",
                            encoding = "UTF-8",
                            sep = " "))

audiovisual = as.tibble(read.csv("youtube-personality/YouTube-Personality-audiovisual_features.csv",
                                 encoding = "UTF-8",
                                 sep = " "))

transcripts = as.tibble(read.csv("youtube-personality/transcripts.csv",
                                 encoding = "UTF-8",
                                 sep = ","))

transcripts$index = as.character(transcripts$index)

transcripts =
  transcripts %>%
  rename(vlogId = index,
         transcript = X0) %>%
  mutate(vlogId = substr(vlogId, 1, nchar(vlogId)-4))

personality$vlogId = as.character(personality$vlogId)
gender$vlogId = as.character(gender$vlogId)
audiovisual$vlogId = as.character(audiovisual$vlogId)

training_data = 
  personality %>%
  left_join(gender, on = c("vlogID" = "vlogID")) %>%
  left_join(audiovisual, on = c("vlogID" = "vlogID")) %>%
  left_join(transcripts, on = c("clogID" = "vlogID"))

#predictors based on transcripts

#establish wordcount function
wordcount <- function(vector_string,path_txt_file, sep)
{
  library(stringr)
  wordlist <- as.vector(colnames(read.csv(path_txt_file,sep=sep)))
  
  count_vector <- vector(length = length(vector_string))
  
  for (j in 1:length(vector_string))
  {
    sum <- 0
    for (i in 1:length(wordlist))
    {
      sum <- sum + str_count(vector_string[j],wordlist[i])
    }
    count_vector[j] <- sum
  }
  return(count_vector)
}

#relative frequency of words associated with Big Five (input library can be modified)
training_data$words_Extraversion <- wordcount(training_data$transcript, "words E.txt", sep = ",") / str_count(training_data$transcript)
training_data$words_Neuroticism <- wordcount(training_data$transcript, "words N.txt", sep = ",") / str_count(training_data$transcript)
training_data$words_Openness <- wordcount(training_data$transcript, "words O.txt", sep = ",") / str_count(training_data$transcript)
training_data$words_Agreeableness <- wordcount(training_data$transcript, "words A.txt", sep = ",") / str_count(training_data$transcript)
training_data$words_Conscientiousness <- wordcount(training_data$transcript, "words C.txt", sep = ",") / str_count(training_data$transcript)

training_data$words_nExtraversion <- wordcount(training_data$transcript, "words negative E.txt", sep = ",") / str_count(training_data$transcript)
training_data$words_nNeuroticism <- wordcount(training_data$transcript, "words negative N.txt", sep = ",") / str_count(training_data$transcript)
training_data$words_nOpenness <- wordcount(training_data$transcript, "words negative O.txt", sep = ",") / str_count(training_data$transcript)
training_data$words_nAgreeableness <- wordcount(training_data$transcript, "words negative A.txt", sep = ",") / str_count(training_data$transcript)
training_data$words_nConscientiousness <- wordcount(training_data$transcript, "words negative C.txt", sep = ",") / str_count(training_data$transcript)


#sentiment scores from syuzhet package - positive = positive emotions, negative = negative emotions

library(syuzhet)

training_data$sentiment <- get_sentiment(as.vector(training_data$transcript), method = "afinn")
#method is interchangable within the syuzhet package; maybe also adjust for transcript length


#add word count for sentiment categories from nrc - scores for each transcript ranging from 0+
nrc_sentiments = get_nrc_sentiment(as.vector(training_data$transcript))

training_data$anger <- nrc_sentiments[,1]
training_data$anticipation <- nrc_sentiments[,2]
training_data$disgust <- nrc_sentiments[,3]
training_data$fear <- nrc_sentiments[,4]
training_data$joy <- nrc_sentiments[,5]
training_data$sadness <- nrc_sentiments[,6]
training_data$surprise <- nrc_sentiments[,7]
training_data$trust <- nrc_sentiments[,8]
