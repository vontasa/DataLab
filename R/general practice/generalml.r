library(ggplot2)
library(nnet)         # multinom
library(rpart) # rf
library(e1071)        # svm
library(caret)
library(GGally)
# First look
head(iris)
summary(iris)
sum(is.na(iris))

ggpairs(iris, aes(colour = Species, alpha = 0.4))

set.seed(100)
x_col <- c(1:4)
y_col <- 5
train_index <- sample(1:nrow(iris), 0.8 * nrow(iris))
test_index <- setdiff(1:nrow(iris), train_index)

train<- iris[train_index, ]
test <- iris[test_index, ]

train[, x_col] <- scale(train[, x_col])
test[, x_col] <- scale(test[, x_col])

# Formula
formula <- as.formula('Species ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width')
# Multinormial
multinom.fit <- multinom(formula, data=train, family = "binomial")
summary(multinom.fit)
# confusion matrix
table(predict(multinom.fit, test[,x_col], type = 'class'), test$Species)
AIC(multinom.fit)
BIC(multinom.fit)
confusionMatrix(predict(multinom.fit, test[,x_col], type = 'class'), test$Species)

# Randomforest
rf.fit <- rpart(formula,data=train)
summary(rf.fit)
table(predict(rf.fit, test[,x_col], type = 'class'), test$Species)
confusionMatrix(predict(rf.fit, test[,x_col], type = 'class'), test$Species)

# SVM
svm.fit<- svm(formula, data = train)
summary(svm.fit)
table(predict(svm.fit, test[,x_col], type = 'class'), test$Species)
confusionMatrix(predict(svm.fit, test[,x_col], type = 'class'), test$Species)

