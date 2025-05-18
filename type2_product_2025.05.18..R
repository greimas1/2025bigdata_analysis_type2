library(dplyr)
library(randomForest)
library(caret)

rdata <- read.csv("product.csv", fileEncoding="euc-kr")
str(rdata)
#rdata <- ifelse(rdata$성별=="남자",1,0)
rdata$성별 <- ifelse(rdata$성별=="남자",1,0)


str(rdata)

data <- rdata[,c("쇼핑액1월","쇼핑액2월","쇼핑액3월","대표제품이름","소득","성별")]

#data <- rdata %>% select(쇼핑액1월 : 소득)
#data <- rdata[,c("쇼핑액1월","쇼핑액2월","쇼핑액3월","대표제품이름","소득")]
#data <- rdata(,c["쇼핑액1월","쇼핑액2월","쇼핑액3월","대표제품이름","소득"])
#data <- rdata[,c("쇼핑액1월","쇼핑액2월","쇼핑액3월","대표제품이름","소득")]

str(data)

idx <- sample(1:nrow(data), nrow(data)*0.7)
train <- data[idx,]
test <- data[-idx,]


md <- randomForest(성별~쇼핑액1월+쇼핑액2월+쇼핑액3월+대표제품이름+소득, data=train, ntree = 300)
pred <- predict(md, newdata = test)
#typeof(pred)
#str(pred)
#is.vector(pred)

pred <- ifelse(pred>0.5,1,0)

cm <- confusionMatrix(as.factor(pred), reference=as.factor(test$성별))
print(cm)
#recall,sensitivity=0.2727
#pos_pred 0.2727
#neg_pred 0.5294

recall <- cm$byClass["Sensitivity"]
pos_prec <- cm$byClass["Pos Pred Value"]
neg_prec <- cm$byClass["Neg Pred Value"]

pos_f1 <- 2*(pos_prec*recall)/(pos_prec+recall)
neg_f1 <- 2*(neg_prec*recall)/(neg_prec+recall)

f1_macro <- mean(pos_f1, neg_f1)
print(round(f1_macro,3))
#0.273