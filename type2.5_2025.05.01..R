library(dplyr)
library(caret)
library(randomForest)

rdata1 <- read.csv("P230604-01.csv")
rdata2 <- read.csv("P230604-02.csv")

#str(rdata1)

rdata1$price_range <- as.factor(rdata1$price_range)

#idx <- 1:sample(nrow(rdata1), nrow(rdata1))
#idx <- sample(1:nrow(rdata1), nrow(rdata1))
idx <- sample(1:nrow(rdata1), nrow(rdata1)*0.75)

train1 <- rdata1[idx,]
valid1 <- rdata1[-idx,]

md1 <- randomForest(price_range~., data =train1, ntree = 300 )
pred1 <- predict(md1, newdata = valid1, ntree = 300, type = "response")
#counfusionMatrix(pred1, reference = valid1$price_range)
confusionMatrix(pred1, reference = valid1$price_range)

#무슨 문제이지??

md2 <- randomForest(price_range~., data =rdata1, ntree = 300 )
#pred2 <- predict(md2, newdata = rdata2, ntree = 300, type = "response")
pred2 <- predict(md2, newdata = rdata2[,-1], ntree = 300, type = "response")
#rdata2에는 id 열이 있지만 price_range는 없음 → 
#그래서 예측이 잘 작동할 수 있지만 모델이 학습했던 열과 일치하지 않으면 경고가 뜰 수 있습니다.

result <- data.frame(pred2)
colnames(result) = c("pred")

print(result)

write.csv(result, "수험번호2.5_2025.05.01.csv", row.names=FALSE)

#답지 코드와 잘 일치하지 않음