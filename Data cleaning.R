data <- read.csv("cleaned_data.csv")
View(data)
dim(data)


##Cleaning for Game.Features
library(dplyr)
data$Game.Features <- gsub("\\[", "", data$Game.Features)
data$Game.Features <- gsub("\\]", "", data$Game.Features)
tmp1 <- unlist(strsplit(data$Game.Features, ","))
uniq_feature <- unique(tmp1)
data1 <- data


for (features in uniq_feature){
  data1[[features]] <- 0
}
dim(data1)
View(data1)

tmp2 <- data1
for (features in uniq_feature){
  tmp2[,features] <- ifelse(grepl(features, tmp2$Game.Features), 1, 0)
}

tmp3 <- tmp2[,19:77]
column_names <- colnames(tmp3)
new_column_names <- paste("Game.Feature.", column_names, sep = "")
colnames(tmp2)[19:77] <- new_column_names

##tmp2 the latest version of dataset

write.csv(tmp2,file = "cleaned_data2.csv")









