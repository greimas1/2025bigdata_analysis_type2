library(caret)
library(randomForest)

rdata1 <- read.csv("P220404-01.csv")
#str(rdata1)

rdata1$Segmentation <- as.factor(rdata1$Segmentation) 

rdata2 <- read.csv("P220404-02.csv")

#str(rdata2)

idx <- sample(1:nrow(rdata1), nrow(rdata1)*0.75)

train <- rdata1[idx,]
valid <- rdata1[-idx,]

md <- randomForest(Segmentation ~., data = train[,-1], ntree = 300)

pred <- predict(md, newdata = valid[,-1])

confusionMatrix(pred, reference = valid[,-1]$Segmentation)

md <- randomForest(Segmentation ~., data = rdata1[,-1], ntree = 300)

pred <- predict(md, newdata = rdata2[,-1])

print(head(pred,2))

#names(rdata2)

result <- data.frame(rdata2$ID, pred)
colnames(result) <- c("ID", "pred")

#head(result,2)

write.csv(result, "수험번호.csv", row.names = FALSE)
