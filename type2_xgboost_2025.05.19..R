library(dplyr)
library(xgboost)
library(caret)

rdata <- read.csv("product.csv", fileEncoding="euc-kr")
#str(rdata)
rdata$성별 <- ifelse(rdata$성별=="남자",1,0)
#str(rdata)
data <- rdata %>% select(쇼핑액1월:성별)
#str(data)
set.seed

idx <- sample(1:nrow(data), nrow(data)*0.7)
train <- data[idx,]
test <- data[-idx,]

#train_x <- train %>% select(쇼핑액1월:소득)
train_x <- as.matrix(train %>% select(쇼핑액1월:소득))
train_y <- train$성별

#test_x <- test %>% select(쇼핑액1월:소득)
test_x <- as.matrix(test %>% select(쇼핑액1월:소득))
test_y <- test$성별

md <- xgboost(data=train_x, label=train_y,
              objective="binary:logistic",
              nrounds=500,
              max_depth=6,
              eta=0.3,
              eval_metric="logloss",
              verbose=0)

pred <- predict(md, newdata=test_x)
#is.vector(pred)
pred <- ifelse(pred>0.5,1,0)

cm <- confusionMatrix(as.factor(pred), reference=as.factor(test_y))
print(cm)

recall <- cm$byClass["Sensitivity"]
pos_pre<- cm$byClass["Pos Pred Value"]
neg_pre <- cm$byClass["Neg Pred Value"]

f1_pos <- (2*pos_pre*recall)/(pos_pre + recall)
f1_neg <- (2*neg_pre*recall)/(neg_pre + recall)
f1_macro <- mean(f1_pos, f1_neg)
print(round(f1_macro,3))
#0.455
