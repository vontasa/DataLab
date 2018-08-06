# -----------------------------------------------------------------------------
# General r ml code pieces
# Yan Wang
# Date: 8/3/2018
# Description: Basic code and functions for naive ML 
# Appendix:
# An intro to ML in R: https://lgatto.github.io/IntroMachineLearningWithR/unsupervised-learning.html#k-means-clustering
# Find na: https://sebastiansauer.github.io/sum-isna/
# Kaggle ml intro: https://www.kaggle.com/camnugent/introduction-to-machine-learning-in-r-tutorial
# Little book of R for TS: http://a-little-book-of-r-for-time-series.readthedocs.io/en/latest/
# caret intro document: http://topepo.github.io/caret/index.html
# -----------------------------------------------------------------------------

if(F){ # choose to install the packages
  install.packages("ggplot2")
  install.packages("nnet")
  install.packages("rpart")
  install.packages("e1071")
  install.packages("caret")
  install.packages("GGally")
  install.packages("tidyr")
  install.packages("dplyr")
  install.packages("corrplot")
}

library(ggplot2)  # general plotting
library(nnet)     # multinom
library(rpart)    # rf
library(e1071)    # svm
library(caret)    # evaluate model
library(GGally)
library(tidyr)    # reshape table
library(dplyr)    # manipulate table
library(corrplot) # correlation plot
# First look
head(iris)
summary(iris)

# ---------------------------
# Find NA
# ---------------------------
sum(is.na(iris)) # Count total NA
# Count NA by column method 1: sapply
sapply(iris, function(x) sum(is.na(x))) 
# Count NA by column method 2: dplyr
iris %>%
  select(everything()) %>%  # replace to your needs
  summarise_all(funs(sum(is.na(.))))
# For more ways to  counting NA refer to https://sebastiansauer.github.io/sum-isna/

# ---------------------------
# Remove na and null
# ---------------------------
# Select rows all columns are not na
iris[complete.cases(iris), ]
# Select rows only if partial columns are not na
iris[complete.cases(iris[, 2:3]), ]

# ---------------------------
# Imputation
#----------------------------
# Imput missing value by median
iris$Sepal.Length[is.na(iris$Sepal.Length)] = median(iris$Sepal.Length , na.rm = TRUE)

# ---------------------------
# Naive outlier remove
# ---------------------------
# Filter by value
subset(iris, Petal.Length > 6 & Petal.Width > 2, select = Sepal.Width:Petal.Width)

# Filter by quantile
subset(iris, Petal.Length < quantile(Petal.Length, 0.95) & 
             Petal.Length > quantile(Petal.Length, 0.05) & 
             Petal.Width > 2.3, select = Sepal.Width:Petal.Width)
# ---------------------------
# tidyr
# ---------------------------

# gather() convert header to value. Convert any column except Species to key (column name of headers)-value (column name of value) pair
# arrange() sort by column. Species asc, type desc
col2val<-iris %>%
  gather(type, value, -Species, na.rm=T) %>%
  arrange(Species, desc(type))
head(col2val)

val2col<-col2val %>%
  spread(type, value)
head(val2col)
# ---------------------------
# dplyr demo
#   mutate() adds new variables that are functions of existing variables
#   select() picks variables based on their names.
#   filter() picks cases based on their values.
#   summarise() reduces multiple values down to a single summary.
#   arrange() changes the ordering of the rows.
# ---------------------------
# Aggregate
iris %>%
  group_by(Species)%>%
  summarise(
    count = n(),
    Petal.Length.mean = mean(Petal.Length, na.rm=T)
  ) %>%
  filter(Species!="setosa" )
  
# ratio of value and mean value by Species
iris %>%
  group_by(Species)%>%
  mutate(percent = Petal.Length/mean(Petal.Length, na.rm=T)) %>%
  filter( Species!="setosa" && 
          Sepal.Length >= mean(Sepal.Length, na.rm=T))


# inner_join, left_join, right_join, full_join
# pair plot
ggpairs(iris, aes(colour = Species, alpha = 0.4))

# Combine histogram and density plot
p<-ggplot(iris, aes(x=Petal.Width, color=Species, fill=Species)) + 
  geom_histogram(aes(y=..density..), fill="white") +
  geom_density(alpha=.2)
p

# Split plot into panel
p<-ggplot(iris, aes(x=Petal.Width, color=Species, fill=Species)) + 
  geom_histogram(aes(y=..density..), fill="white") +
  geom_density(alpha=.2) +
  facet_grid(Species ~.) +
  geom_vline()
p


# ---------------------------
# Prepare training and test set
# ---------------------------
set.seed(100)
x_col <- c(1:4)
y_col <- 5
train_index <- sample(1:nrow(iris), 0.8 * nrow(iris))
test_index <- -train_index

train<- iris[train_index, ]
test <- iris[test_index, ]
(nrow(train)+nrow(test)) == nrow(iris) # check if train + test = all
colMeans(train[, x_col])
# Normalize the variable columns between -1 - 1
train[, x_col] <- scale(train[, x_col])
test[, x_col] <- scale(test[, x_col])

# Basic box plot
p <- ggplot(train, aes(x=Species, y=Petal.Length)) + 
  geom_boxplot(outlier.color = 'red', outlier.shape = 1)
p

# Correlation matrix and plot
M<-cor(iris[,x_col])
head(round(M,2))
corrplot(M, method="color", type ="upper", tl.srt=45, tl.col="black",diag=FALSE,addCoef.col = "black")

# ---------------------------
# Prediction
# ---------------------------
# Prediction formular
formula_pred <- as.formula('Sepal.Length ~ Sepal.Width + Petal.Length + Petal.Width + Species')

# Linear model
lm.fit<- lm(formula_pred, data = train)
summary(lm.fit)
null.fit <- lm('Sepal.Length ~ 1', data = train)
full.fit <- lm('Sepal.Length ~ .', data = train)
print(full.fit)

# Use caret to evaluate
set.seed(42)
model.lm <- train(formula_pred, data=iris,
               method = "lm", 
               metric="RMSE",
               trControl = trainControl(method = "cv", number = 10))
model.lm

# Linear model - var selection
fwd.model = step(null.fit, direction='forward', scope=list(upper=full.fit))
print(fwd.model)
bck.model = step(null.fit, direction='both', scope=list(upper=full.fit))
print(bck.model)
# ---------------------------
# Time Series
# ---------------------------


# ---------------------------
# Classification
# ---------------------------
# Formula
formula <- as.formula('Species ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width')
# Multinormial
multinom.fit <- multinom(formula, data=train, family = "binomial")
print(multinom.fit)
# confusion matrix
table(predict(multinom.fit, test[,x_col], type = 'class'), test$Species)
AIC(multinom.fit)
BIC(multinom.fit)
confusionMatrix(predict(multinom.fit, test[,x_col], type = 'class'), test$Species)

# Randomforest
rf.fit <- rpart(formula,data=train)
print(rf.fit)
table(predict(rf.fit, test[,x_col], type = 'class'), test$Species)
# Print variable importance
rf.fit$variable.importance
#confusionMatrix(predict(rf.fit, test[,x_col], type = 'class'), test$Species)
model.rf <- train(formula, data=iris,
                  method = "rf", 
                  trControl = trainControl(method = "cv", number = 10))
model.rf

# SVM
svm.fit<- svm(formula, data = train, cross = 10) # 10 fold-cv
print(svm.fit)
summary(svm.fit)
table(predict(svm.fit, test[,x_col], type = 'class'), test$Species)
#confusionMatrix(predict(svm.fit, test[,x_col], type = 'class'), test$Species)

# Tune SVM 
tuned.svm.fit = tune.svm(formula, data = train, gamma = 10^-2, cost = 10^2, tunecontrol=tune.control(cross=10))
summary(tuned.svm.fit)
