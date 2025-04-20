library(caret)
library(randomForest)
library(ModelMetrics)

rdata1 <- read.csv("P220504-01.csv")

summary(rdata1)
str(rdata1)

rdata2 <- read.csv("P220504-02.csv")

summary(rdata2)
str(rdata2)

idx <- sample(1:nrow(rdata1), nrow(rdata1)*0.75)
train <- rdata1[idx,]
valid <- rdata1[-idx,]

md <- randomForest(price~., data = train, ntree = 300)

pred <- predict(md, newdata = valid)

print(rmse(actual = valid$price, predicted=pred))

md<- randomForest(price~., data = rdata1, ntree = 300)

pred <- predict(md, newdata = rdata2)

print(head(pred,2))

result <- data.frame(pred)

colnames(result) <- c("pred")

#head(result,2)

write.csv(result, "수험번호.csv", row.names = FALSE)
