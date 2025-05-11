
library(dplyr)
library(caret)
library(randomForest)

rdata <- read.csv("product.csv", fileEncoding = "euc-kr")
#str(rdata)
#summary(rdata)

#rdata$성별 <- ifelse(rdata$성별=="남자",1,0)
rdata$성별 <- as.factor(ifelse(rdata$성별=="남자",1,0))

idx <- sample(1:nrow(rdata), nrow(rdata)*0.7)
train <- rdata[idx,]
test <- rdata[-idx,]

#md <- randomForest(성별 ~ 쇼핑액1월+쇼핑액2월+쇼핑액3월+대표제품이름+소득, data=train, ntree=500)
md <- randomForest(성별 ~ 쇼핑액1월+쇼핑액2월+쇼핑액3월+소득, data=train, importance=TRUE, ntree=500)
pred <- predict(md, newdata=test)

typeof(pred)
#pred <- ifelse(pred>=0.5, 1, 0)

#confusionMatrix(pred, reference=test$성별)
#confusionMatrix(pred, reference=test$성별, mode="pre_recall")
cm <- confusionMatrix(pred, reference=test$성별, mode="prec_recall")

#cm$byClass["F1"]
#cm$byClass[,"F1"]

pos_precision <- cm$byClass["Pos Pred Value"]
recall <- cm$byClass["Sensitivity"]
pos_f1 <- 2*pos_precision*recall/(pos_precision+recall)

neg_precision <- cm$byClass["Neg Pred Value"]
recall <- cm$byClass["Sensitivity"]
neg_f1 <- 2*neg_precision*recall/(neg_precision+recall)

macro_f1 <- mean(pos_f1, neg_f1)
result <- round(macro_f1,3)
print(result)
#0.526
---------------------------------------------------------------------------------------------------




























library(dplyr)
library(caret)
library(randomForest)

rdata <- read.csv("product.csv", fileEncoding = "euc-kr")
#str(rdata)
#summary(rdata)

#rdata$성별 <- ifelse(rdata$성별=="남자",1,0)
rdata$성별 <- as.factor(ifelse(rdata$성별=="남자",1,0))

idx <- sample(1:nrow(rdata), nrow(rdata)*0.7)
train <- rdata[idx,]
test <- rdata[-idx,]

#md <- randomForest(성별 ~ 쇼핑액1월+쇼핑액2월+쇼핑액3월+대표제품이름+소득, data=train, ntree=500)
md <- randomForest(성별 ~ 쇼핑액1월+쇼핑액2월+쇼핑액3월+대표제품이름+소득, data=train, importance=TRUE, ntree=500)
pred <- predict(md, newdata=test)

typeof(pred)
#pred <- ifelse(pred>=0.5, 1, 0)

confusionMatrix(pred, reference=test$성별)

--------------------------------------------------------------------------------------------------------------------------------------------
library(dplyr)
library(caret)
library(randomForest)

rdata <- read.csv("product.csv", fileEncoding = "euc-kr")
#str(rdata)
#summary(rdata)

#rdata$성별 <- ifelse(rdata$성별=="남자",1,0)
rdata$성별 <- as.factor(ifelse(rdata$성별=="남자",1,0))

idx <- sample(1:nrow(rdata), nrow(rdata)*0.7)
train <- rdata[idx,]
test <- rdata[-idx,]

#md <- randomForest(성별 ~ 쇼핑액1월+쇼핑액2월+쇼핑액3월+대표제품이름+소득, data=train, ntree=500)
md <- randomForest(성별 ~ 쇼핑액1월+쇼핑액2월+쇼핑액3월+대표제품이름+소득, data=train, importance=TRUE, ntree=500)
pred <- predict(md, newdata=test)

typeof(pred)
#pred <- ifelse(pred>=0.5, 1, 0)

confusionMatrix(pred, reference=test$성별)

