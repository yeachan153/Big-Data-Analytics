#create overview over correlation between features and traits

length <- length(training_data[,c(1:32,34:length(training_data))]) - 7

E.table <- as.data.frame(matrix(NA, nrow = length, ncol = 3))

for(i in 1:length){
  E.table[i,1] <- names(training_data[,c(1:32,34:length(training_data))][7+i])
  E.table[i,2] <- cor.test(training_data$Extr,as.vector(unlist(training_data[,c(1:32,34:length(training_data))][,7+i])))$estimate
  E.table[i,3] <- cor.test(training_data$Extr,as.vector(unlist(training_data[,c(1:32,34:length(training_data))][,7+i])))$p.value
}

N.table <- as.data.frame(matrix(NA, nrow = length, ncol = 3))

for(i in 1:length){
  N.table[i,1] <- names(training_data[,c(1:32,34:length(training_data))][7+i])
  N.table[i,2] <- cor.test(training_data$Emot,as.vector(unlist(training_data[,c(1:32,34:length(training_data))][,7+i])))$estimate
  N.table[i,3] <- cor.test(training_data$Emot,as.vector(unlist(training_data[,c(1:32,34:length(training_data))][,7+i])))$p.value
}

O.table <- as.data.frame(matrix(NA, nrow = length, ncol = 3))

for(i in 1:length){
  O.table[i,1] <- names(training_data[,c(1:32,34:length(training_data))][7+i])
  O.table[i,2] <- cor.test(training_data$Open,as.vector(unlist(training_data[,c(1:32,34:length(training_data))][,7+i])))$estimate
  O.table[i,3] <- cor.test(training_data$Open,as.vector(unlist(training_data[,c(1:32,34:length(training_data))][,7+i])))$p.value
}

C.table <- as.data.frame(matrix(NA, nrow = length, ncol = 3))

for(i in 1:length){
  C.table[i,1] <- names(training_data[,c(1:32,34:length(training_data))][7+i])
  C.table[i,2] <- cor.test(training_data$Cons,as.vector(unlist(training_data[,c(1:32,34:length(training_data))][,7+i])))$estimate
  C.table[i,3] <- cor.test(training_data$Cons,as.vector(unlist(training_data[,c(1:32,34:length(training_data))][,7+i])))$p.value
}


A.table <- as.data.frame(matrix(NA, nrow = length, ncol = 3))

for(i in 1:length){
  A.table[i,1] <- names(training_data[,c(1:32,34:length(training_data))][7+i])
  A.table[i,2] <- cor.test(training_data$Agr,as.vector(unlist(training_data[,c(1:32,34:length(training_data))][,7+i])))$estimate
  A.table[i,3] <- cor.test(training_data$Agr,as.vector(unlist(training_data[,c(1:32,34:length(training_data))][,7+i])))$p.value
}









