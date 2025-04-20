library(caret)
library(randomForest)

rdata1 <- read.csv("P210304-01.csv")
#str(rdata1)
#names(rdata1)

rdata1$TravelInsurance <- as.factor(rdata1$TravelInsurance)
#str(rdata1)

rdata2 <- read.csv("P210304-02.csv")
#str(rdata2)
#names(rdata2)

idx <- sample(1:nrow(rdata1), nrow(rdata1)*0.75)
train <- rdata1[idx,]
valid <- rdata1[-idx,]
md <- randomForest(TravelInsurance ~., data = train[, -1], ntree = 300, probability = TRUE)

pred <- predict(md, newdata = valid[,-1], probability = TRUE, type = "response")

confusionMatrix (data = pred, reference = valid[,-1]$TravelInsurance)

md <- randomForest(TravelInsurance ~., data = rdata1[,-1], probabilty = TRUE, ntree = 3000)
pred <- predict(md, newdata = rdata2[,-1], probabilty = TRUE, type = "prob")
#print(head(pred,2))

result <- data.frame(rdata2$X, pred[,2])
#head(result,2)
colnames(result) <- c("index", "y_pred")
#head(result,2)

write.csv(result,"수험번호.csv", row.names = FALSE)
