library(dplyr)
library(randomForest)
#library(caret)
install.packages(caret)
library(caret)

rdata1 <- read.csv("P210204-01.csv")
#str(rdata1)
#idx <- sample(1:nrow(rdata1), nrow(rdata1))
idx <- sample(1:nrow(rdata1), nrow(rdata1)*0.8)
train <- rdata1[idx,]
valid <- rdata1[-idx,]

md <- randomForest(Reached.on.Time_Y.N ~., data = train, ntree = 300)
pred <- predict(md, newdata = valid[,-1], type="response")
#confusionMartrix(pred, reference = valid[,-1]$Reached.on.Time_Y.N)
confusionMatrix(pred, reference = valid[,-1]$Reached.on.Time_Y.N)
