library(dplyr)
library(caret)
library(randomForest)
#library(modelMetrics)
library(ModelMetrics)
#install.packages("modelMertrics")
rdata1 <- read.csv("P220504-01.csv")
rdata2 <- read.csv("P220504-02.csv")

#str(rdata1)
#head(rdata1,10)
#summary(rdata1)

#str(rdata2)

idx <- sample(1:nrow(rdata1), nrow(rdata1)*0.75)
#train <- rdata1[idx,]
#valid <- rdata1[-idx,]
train1 <- rdata1[idx,]
valid1 <- rdata1[-idx,]
#그런데 코드 어디에도 **train1**을 만든 적이 없습니다.
#당신이 만든 데이터셋 이름은 **train**입니다:


md1 <- randomForest(price~., data=train1, ntree = 300)
pred1 <- predict(md1, newdata = valid1, ntree = 300)

#rmse(actual=valid1, predicted=pred1)
rmse(actual=valid1$price, predicted=pred1)
print(rmse(actual=valid1$price, predicted=pred1))
#2771.64


md2 <- randomForest(price~., data=rdata1, ntree = 300)
pred2 <- predict(md2, newdata = rdata2, ntree = 300)
pred <- data.frame(pred2)
colnames(pred) = c("pred")

result <- pred
#write.csv(result, "수험번호2.4_2025.05.02.,csv", row.names=FALSE)
write.csv(result, "수험번호2.4_2025.05.02.csv", row.names=FALSE)
