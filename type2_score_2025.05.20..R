library(dplyr)

rdata <- read.csv("score.csv", fileEncoding="euc-kr")

str(rdata)
summary(rdata)
rdata$standard <- (rdata$score-mean(rdata$score))/sd(rdata$score)
str(rdata)

print(sum(rdata$standard>=0.8))
#805

data_year <- rdata[rdata$year==2022,]
#data_year_1 <- rdata %>% filter(year==2022)

df <- data_year %>% select(-year, -subject)
#df_1 <- subset(data_year, select=-c(year, subject))

df$type <- trimws(df$type)
unique(df$type)

str(df$type)

#df_newdf[df$type %in% c('간호사', '간호조무사', '요양보호사', '의사'), ]
df_new <- df %>% filter(type=="간호사" | type=="간호조무사" | type=="요양보호사" | type=="의사")


corr <- function(type1, type2, anadata) {
  subset1 <- anadata[anadata$type == type1, "score"]
  subset2 <- anadata[anadata$type == type2, "score"]
  min_length <- min(length(subset1), length(subset2))
  subset1 <- subset1[1:min_length]
  subset2 <- subset2[1:min_length]
  correlation <- cor(subset1, subset2)
  return(data.frame(subset1 = type1, subset2 = type2, correlation=correlation))}


corrdata <- rbind(corr("간호사", "간호조무사", df_new),
                  corr("간호사", "요양보호사", df_new),
                  corr("간호사", "의사", df_new),
                  corr("간호조무사", "요양보호사", df_new),
                  corr("간호조무사", "의사", df_new),
                  corr("요양보호사", "의사", df_new))
print(corrdata)


filtered <- corrdata[!is.na(corrdata$correlation),]
print(filtered)

max_row <- filtered[which.max(abs(filtered$correlation)),]
print(max_row)

cat("Type1 :", max_row$subset1, "\n")
cat("type2 :", max_row$subset2, "\n")
cat("correlation :", max_row$correlation, "\n")

head(rdata)

rdata$type <- trimws(rdata$type)
dfnew <- rdata %>% filter(type=="의사")
nrow(dfnew)
str(dfnew)
q25 <- quantile(dfnew$score, 0.25)
q75 <- quantile(dfnew$score, 0.75)
iqr <- q75 - q25
print(iqr)
print(sum(dfnew$score >= q75 + 0.1*iqr))
