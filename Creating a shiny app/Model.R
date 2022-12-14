##################################
## Importing libraries

library(RCurl)
library(randomForest)
library(caret)
library(readr)

df <- read.csv("australian.csv")


TrainigIndex <- createDataPartition(df$Y, p = 0.8, list = FALSE)
TrainingSet <- df[TrainigIndex,]
TestingSet <- df[-TrainigIndex,]
str(TrainingSet)

write.csv(TrainingSet, "training.csv")
write.csv(TestingSet, "testing.csv")


TrainSet <- read.csv("training.csv", header = TRUE)
head(TrainSet)
TrainSet <- TrainSet[,-1]
str(TrainSet)
TrainSet$Y = as.factor(TrainSet$Y)

#df$default <- as.factor(df$default)
str(TrainSet)
model <- randomForest(Y ~., data = TrainSet, ntree = 500, mtry = 4, importance = TRUE)


saveRDS(model, "model.rds")
