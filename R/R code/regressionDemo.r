#************************************************
# Linear Regression Demo
# Edward Wang
# 5/20/2014
# Description: using iris dataset show how to do
# linear regression, variable selection and cross-validation
#************************************************
# If you don't have the following packages, install them
#install.packages('ggplot2')
#install.packages('DAAG')
#install.packages('GGally')

library(ggplot2) # awesome plotting package in R
library(DAAG)
library(GGally)
# Take a look of the dataset
head(iris)    # top 6 rows of a dataframe
str(iris)     # structure of data
summary(iris) # statistical sumary
#plotmatrix(iris[,1:4])+geom_smooth(method="lm") # (ggplot2) take a first look of the data
ggpairs(iris,
        columns=1:5,
        axisLabels='none',
        colour='Species') # (ggplot2) take a first look of the data

# Get a close look at the Petal Length vs Sepal Length
qplot(Sepal.Length, Petal.Length, data = iris, color = Species, shape=Species, size=30,
      xlab = "Sepal Length", ylab = "Petal Length",
      main = "Sepal vs. Petal Length in Fisher's Iris data")

set.seed(1) # fake randomness to make sure everyone get same result
testidx<- sample.int(nrow(iris),nrow(iris)/5) # ramdom sample the index of rawData
testidx # take a loot of the index of test dataset

iristest<-iris[testidx,]   # Test set
iristrain<-iris[-testidx,] # Training set is the complement of test set

# use all variables to fit the model
f_isSetosa<-Sepal.Length~Species+Sepal.Width+Petal.Length+Petal.Width
logreg<-lm(f_isSetosa,data=iristrain)
(result <- predict(logreg, newdata=iristest, type='response'))
# make the result looks well organized
(testresult<-cbind(Predicted=result,Actual=iristest$Sepal.Length,Error=iristest$Sepal.Length-result))
summary(testresult)
summary(logreg)
# VARIABLE SELECTION

# Stepwise selection by AIC (set k=log(nrow(iristrain)) to use BIC) 
selectedlogreg<-step(logreg, data=iristrain, direction="both",  scope=list(lower=Sepal.Length~1, upper=logreg))
result <- predict(selectedlogreg, newdata=iristest, type='response')
testresult_selected<-cbind(Predicted=result,Actual=iristest$Sepal.Length,Error=iristest$Sepal.Length-result)
summary(testresult_selected)
summary(selectedlogreg)$r.squared
plot(selectedlogreg)
# Naive model: 
naivelogreg<-lm(Sepal.Length~Sepal.Width, data=iristrain)
result <- predict(naivelogreg, newdata=iristest, type='response')
testresult_naive<-cbind(Predicted=result,Actual=iristest$Sepal.Length,Error=iristest$Sepal.Length-result)
summary(testresult_naive)
summary(naivelogreg)
# Conclusion is either Sepal.Width,Sepal.Length and Petal.Width is irrelavant to the species or are highly corelatted with Petal.Length


# CROSS VALIDATION
library(boot) # bootstrap library
cvSelected<-cv.lm(iristrain, selectedlogreg, m=10, plotit=F, printit=T)
cvLogreg<-cv.lm(iristrain, logreg, m=10, plotit=F, printit=T)
cvNaive<-cv.lm(iristrain, naivelogreg, m=10, plotit=F, printit=T)

attr(cvSelected,'ms')
attr(cvLogreg,'ms')
attr(cvNaive,'ms')

sum(testresult_selected[,'Error']^2)^0.5
sum(testresult[,'Error']^2)^0.5
sum(testresult_naive[,'Error']^2)^0.5
str(logreg)
summary(logreg)$r.squared
summary(selectedlogreg)$r.squared
summary(naivelogreg)$.r.squared
