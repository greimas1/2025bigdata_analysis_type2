library(dplyr)
library(randomForest)
library(caret)

rdata1 <- read.csv("P210204-01.csv")
rdata2 <- read.csv("P210204-02.csv")

#str(rdata1)

rdata1$Reached.on.Time_Y.N <- as.factor(rdata1$Reached.on.Time_Y.N)

idx <- sample(1:nrow(rdata1), nrow(rdata1)*0.75)

train <- rdata1[idx,]
valid <- rdata1[-idx,]

md <- randomForest(Reached.on.Time_Y.N~., data=train[,-1], ntree = 300)
pred <- predict(md, newdata = valid[,-1], ntree = 300, type = "response")
confusionMatrix(pred, reference = valid[,-1]$Reached.on.Time_Y.N)

md2 <- randomForest(Reached.on.Time_Y.N~., data=rdata1[,-1], ntree = 300)
pred2 <- predict(md2, newdata = rdata2[,-1], ntree = 300, type = "prob")

#result <- data.frame(rdata2$ID, pred[,2])
result <- data.frame(rdata2$ID, pred2[,2])
colnames(result) <- c("ID", "pred")
#result

#write.csv(result, "수험번호.csv", row.names="FALSES")
write.csv(result, "수험번호.csv", row.names=FALSE)
