library(caret)
library(randomForest)

rdata1 <- read.csv("P230604-01.csv")

#str(rdata1)
#summary(rdata1)

names(rdata1)

rdata1$price_range <- as.factor(rdata1$price_range)
rdata1$blue <- as.factor(rdata1$blue)
rdata1$dual_sim <- as.factor(rdata1$dual_sim)
rdata1$four_g <- as.factor(rdata1$four_g)
rdata1$n_cores <- as.factor(rdata1$n_cores)
rdata1$three_g <- as.factor(rdata1$three_g)
rdata1$touch_screen <- as.factor(rdata1$touch_screen)
rdata1$wifi <- as.factor(rdata1$wifi)

rdata2 <- read.csv("P230604-02.csv")

#str(rdata2)
#rdata2$price_range <- as.factor(rdata2$price_range)
rdata2$blue <- as.factor(rdata2$blue)
rdata2$dual_sim <- as.factor(rdata2$dual_sim)
rdata2$four_g <- as.factor(rdata2$four_g)
rdata2$n_cores <- as.factor(rdata2$n_cores)
rdata2$three_g <- as.factor(rdata2$three_g)
rdata2$touch_screen <- as.factor(rdata2$touch_screen)
rdata2$wifi <- as.factor(rdata2$wifi)


idx <- sample(1:nrow(rdata1), nrow(rdata1)*0.75)

train <- rdata1[idx,]
valid <- rdata1[-idx,]
md <- randomForest(price_range~., data = train, ntree = 300)

pred <- predict(md, newdata = valid)

confusionMatrix(data = pred, reference = valid$price_range, mode = "everything")


md<- randomForest(price_range~., data = rdata1, ntree = 300)

pred <- predict(md, newdata = rdata2)






