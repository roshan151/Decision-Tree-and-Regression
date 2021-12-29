library(caret)
library(e1071)

Platform_Usage.df <- read.csv("spend21_usage.csv")
Platform_Usage.df <- Platform_Usage.df[ , -c(1)] 

train.index <- sample(c(1:dim(Platform_Usage.df)[1]), dim(Platform_Usage.df)[1]*0.6)  
train.df <- Platform_Usage.df[train.index, ]
valid.df <- Platform_Usage.df[-train.index, ]

logit.reg <- glm(ARR~ ., data = train.df, family = "binomial") 
options(scipen=999)
summary(logit.reg)


#### Table 10.3

# use predict() with type = "response" to compute predicted probabilities. 
logit.reg.pred <- predict(logit.reg, valid.df[, -1], type = "response")

# first 5 actual and predicted records
data.frame(actual = valid.df$ARR[1:5], predicted = logit.reg.pred[1:5])

confusionMatrix(as.factor(ifelse(logit.reg.pred>0.5, 1, 0)), as.factor(valid.df$ARR))
