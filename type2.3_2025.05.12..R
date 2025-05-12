library(dplyr)
library(randomForest)
library(caret)

rdata1 <- read.csv("P220404-01.csv")
rdata2 <- read.csv("P220404-02.csv")

str(rdata1)
rdata1$Segmentation <- as.factor(rdata1$Segmentation)
str(rdata1)

idx <- sample(1:nrow(rdata1), nrow(rdata1)*0.8)
train <- rdata1[idx,-1]
test <- rdata1[-idx,-1]

md1 <- randomForest(Segmentation~., data=train, ntree=300)
pred1 <- predict(md1, newdata = test)
#pred1 <- as.factor(pred1)
#confusionMatrix(pred1, reference=rdata1$Segmentation)
confusionMatrix(pred1, reference=test$Segmentation)
confusionMatrix(pred1, reference=test$Segmentation, mode="everything")
cm <- confusionMatrix(pred1, reference=test$Segmentation)
macro_f1 <- mean(cm$byClass[,"F1"])
print(macro_f1)
#0.5109907

md2 <- randomForest(Segmentation~., data=rdata1[,-1], ntree=300)
pred2 <- predict(md2, newdata=rdata2[,-1])
typeof(pred2)
str(pred2)
#result <- data.frame(rdata2$ID, pred2[,2])
result <- data.frame(rdata2$ID, pred2)
colnames(result) <- c("ID","pred")

write.csv(result,"수험번호20250512.csv",row.names=FALSE)
