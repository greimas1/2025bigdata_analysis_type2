library(caret)
library(randomForest)

rdata1 <- read.csv("P210204-01.csv")

#str(rdata1)
#summary(rdata1)

rdata1$Reached.on.Time_Y.N <- as.factor(rdata1$Reached.on.Time_Y.N)

rdata2 <- read.csv("P210204-02.csv")

#summary(rdata2)
#str(rdata2)

idx <- sample(1:nrow(rdata1), nrow(rdata1)*0.75)

train <- rdata1[idx,]
valid <- rdata1[-idx,]

#md <- randomForest(Reached.on.Time_Y.N~., data=train[,-1], ntree = 300, probability = TRUE)

md <- randomForest(Reached.on.Time_Y.N~., data=train[,-1], ntree = 300)

pred <- predict(md, newdata = valid[,-1], probability = TRUE, type = "response")

confusionMatrix(data = pred, reference = valid[,-1]$Reached.on.Time_Y.N)

md <- randomForest(Reached.on.Time_Y.N~., data=train[,-1], ntree = 300, probability = TRUE)

pred <- predict(md, newdata = rdata2[,-1], probability = TRUE, type = "prob")

print(pred)

result <- data.frame(rdata2$ID, pred[,2])

colnames(result) <- c("ID", "pred")

write.csv(result, "수험번호.csv", row.names = FALSE)
