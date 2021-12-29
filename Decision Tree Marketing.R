library(rpart)
library(rpart.plot)
library(e1071)

Platform_Usage.df <- read.csv("Marketing.csv")
Platform_Usage.df <- Platform_Usage.df[ , -c(1)] 

train.index <- sample(c(1:dim(Platform_Usage.df)[1]), dim(Platform_Usage.df)[1]*0.6)  
train.df <- Platform_Usage.df[train.index, ]
valid.df <- Platform_Usage.df[-train.index, ]

set.seed(1)
cv.ct <- rpart(Revenue ~ ., data = train.df, method = "class", cp = 0.001, minsplit = 10, xval = 5)  # minsplit is the minimum number of observations in a node for a split to be attempted. xval is number K of folds in a K-fold cross-validation.
printcp(cv.ct)  # Print out the cp table of cross-validation errors. The R-squared for a regression tree is 1 minus rel error. xerror (or relative cross-validation error where "x" stands for "cross") is a scaled version of overall average of the 5 out-of-sample errors across the 5 folds.
pruned.ct <- prune(cv.ct, cp = 0.0068729)


printcp(pruned.ct)
prp(pruned.ct, type = 1, extra = 1, under = TRUE, split.font = 1, varlen = -10, 
    box.col=ifelse(pruned.ct$frame$var == "<leaf>", 'gray', 'white')) 

default.ct.point.pred.valid <- predict(cv.ct,valid.df,type = "class")
confusionMatrix(default.ct.point.pred.valid, as.factor(valid.df$Revenue))
