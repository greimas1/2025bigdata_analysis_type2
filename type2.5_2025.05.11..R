library(dplyr)
library(randomForest)
library(caret)
rdata1 <- read.csv("P230604-01.csv") 
rdata2 <- read.csv("P230604-02.csv")

#str(rdata1)
#blue, dual_sim, four_g, three_g, touch_screen, wifi
#price_range
#n_cores <-- 딱히 0,1이 아닌데 어떻게 범주화라고 단정 지을 수 있을까?
#보통 R에서 자동으로 범주화 기능을 쓰면 0,1로 하는 듯..
#다르게 하려면 label Encoding개념을 써야 할 듯..

rdata1$price_range <- as.factor(rdata1$price_range)

#idx <- sample(1:norw(rdata1), norw(rdata1)*0.8)
idx <- sample(1:nrow(rdata1), nrow(rdata1)*0.8)
train <- rdata1[idx,]
test <- rdata1[-idx,]

md1 <- randomForest(price_range~., data = train, ntree = 300)
pred1 <- predict(md1, newdata=test)
#counfusionMatrix(pred1, reference = test$price_range, mode = "everything")
confusionMatrix(pred1, reference = test$price_range, mode = "everything")

md2 <- randomForest(price_range~., data =rdata1, ntree = 300)
pred2 <- predict(md2, newdata=rdata2)

#typeof(pred2)
#typeof(pred2)
#str(pred2)

#result <- data.frame(pred2[,2])
#result <- as.data.frame(pred2[,2])
result <- data.frame(pred2)
colnames(result) <- c("pred")
write.csv(result, "수험번호2025.05.11..csv", row.names=FALSE)