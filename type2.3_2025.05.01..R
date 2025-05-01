library(dplyr)
library(caret)
library(randomForest)

#rdata1 <- read.csv("P220404-01.csv", fileEncoding = "euc-kr")
rdata1 <- read.csv("P220404-01.csv")
rdata2 <- read.csv("P220404-02.csv")

rdata1$Segmentation <- as.factor(rdata1$Segmentation) 
#idx <- 1:sample(nrow(rdata1), nrow(rdata1)*0.75)
idx <- sample(1:nrow(rdata1), nrow(rdata1)*0.75)
train1 <- rdata1[idx,]
valid1 <- rdata1[-idx,]

md1 <- randomForest(Segmentation~., data = train1[,-1], ntree = 300)
#pred1 <- predict(md1, newdata = valid1[,-1], ntree = 300, type = "response" )
pred1 <- predict(md1, newdata = valid1[,-1], ntree = 300)

confusionMatrix(pred1, reference = valid1[,-1]$Segmentation)


md2 <- randomForest(Segmentation~., data = rdata1[,-1], ntree = 300)
pred2 <- predict(md2, newdata = rdata2[,-1], ntree = 300)
#좋은 논리적 접근이에요! 하지만 핵심은 "type = 'response'"가
#pred2를 데이터프레임으로 만들 수 있느냐가 아니라,
#predict() 함수의 반환값이 무엇인지에 따라 
#인덱싱 방식이 달라진다는 점입니다.

#str(pred2)

#result <- data.frame(rdata2$ID, pred2[,1])
result <- data.frame(rdata2$ID, pred2)

#typeof(pred2)
#class(pred2)

colnames(result) <- c("ID", "pred")

write.csv(result, "수험번호20250501.csv", row.names=FALSE)
