library(dplyr)
library(caret)
library(randomForest)

rdata1 <- read.csv("P210304-01.csv")
rdata2 <- read.csv("P210304-02.csv")

#str(rdata1)

rdata1$TravelInsurance <- as.factor(rdata1$TravelInsurance)

idx <- sample(1:nrow(rdata1), nrow(rdata1)*0.75)
train1 <- rdata1[idx,]
valid1 <- rdata1[-idx,]

md1 <- randomForest(TravelInsurance~., data = train1[,-1], ntree = 300)
pred1 <- predict(md1, newdata = valid1[,-1], ntree = 300, type = "response" )
#confusionMatrix(pred1, reference = valid1$TravelInsurance)
confusionMatrix(pred1, reference = valid1[,-1]$TravelInsurance)

md2 <- randomForest(TravelInsurance~., data = rdata1[,-1], ntree = 300)
pred2 <- predict(md2, newdata = rdata2[,-1], ntree = 300, type = "prob" )
#str(rdata2)
#str(pred2)
result <- data.frame(rdata2$X, pred2[,2])
colnames(result) <- c("index", "y_pred")

write.csv(result, "수험번호20250430.csv", row.names=FALSE)
