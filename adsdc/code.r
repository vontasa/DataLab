setwd('E:\\my work\\Github\\DataLab\\adsdc')
library(reshape2)
library(ggplot2)
library(GGally)
library(dplyr)
library(ggfortify)# ggplot PCA plotting
library(e1071)    # SVM library
library(randomForest)
library(DMwR)


raw<-read.csv("dataset_challenge_one.tsv",sep="\t", header=TRUE)
# Remove missing value
sum(!complete.cases(raw))
df<-raw[complete.cases(raw),]         # complete records
var_col<-which(names(df)!="class")    # columns of variables
class_col<-which(names(df)=="class")  # column of class

# Overview:: Variable Correlation
mcor<-cor(df[, var_col])
melt_mcor<-melt(mcor)
melt_mcor<-melt_mcor[order(melt_mcor$value),]

# Plot the correlation distribution
p_histogram_correlation<-ggplot(melt_mcor, aes(x=value)) + 
  geom_histogram()+
  labs(x = "Correlation")+
  ggtitle("Distribution of Attribute Correlation")
p_histogram_correlation
# The mean is close to 0 and like bell shape which means the selected attributes are not heavily correlated.
summary(melt_mcor)
nrow(subset(melt_mcor, value>=0.9 & Var1!=Var2))


normaliy<-sapply(x, function(x) shapiro.test(x)$p.value)
var_norm_df<-data.frame(pvalue=normaliy,dist=ifelse(normaliy>=0.05,"non-normal p>0.05","normal")) 
p_variable_normality<-ggplot(var_norm_df,aes(x=factor(dist))) + geom_bar() +
  labs(x = "Distribution")+
  ggtitle("Count of non-nommal distributed variable")
p_variable_normality



# Outlier detection
outlier.scores <- lofactor(df[, var_col], k=5)
plot(density(outlier.scores))
outliers <- order(outlier.scores, decreasing=T)[1:10]
print(outliers)
labels<-1:nrow(df)
labels[-outliers] <- "."

# Plot the outlier in PC1 vs PC2
fit <- prcomp(df[,var_col], cor=TRUE)
outlier_df<-df
outlier_df$class<-ifelse(labels>=1,-1,outlier_df$class)
outlier_df$class<-as.factor(outlier_df$class)
autoplot(fit, data =outlier_df, colour = 'class',shape=T)


# PCA
fit <- prcomp(df[,var_col], cor=TRUE)
summary(fit) # print variance accounted for 
fit$rotation[,1:2]
plot(fit,type="lines") # scree plot 
fit$scores # the principal components
df$class<-as.factor(df$class)
autoplot(fit, data=df, colour = 'class')
df.pca<-data.frame((fit$x)[,1:2],class=df[,class_col])
corr.pca<-cor(df.pca)
df.pca$class<-as.factor(df.pca$class)
ggpairs(data= df.pca, mapping=aes(color=class))
str(df.pca)

## correaltion between each var and class
varClassCorr<-sapply(df[,var_col], function(x) cor(x,as.numeric(df$class)))
varClassCorr[order(abs(varClassCorr), decreasing = T)[1:2]]
top.var<-order(abs(varClassCorr), decreasing = T)[1:10]
var.sig<-df[,c(order(abs(varClassCorr), decreasing = T)[1:2],class_col)]
ggpairs(data=var.sig, mapping=aes(color=class))
str(var.sig)
plot(varClassCorr[order(abs(varClassCorr), decreasing = T)[1:10]], main="Top 10 variable correlated to class")

# rotation of PC1
PC1=fit$x[,1]
varPC1Corr<-sapply(df[,var_col], function(x) cor(x,PC1))
order(abs(varPC1Corr), decreasing = T)[1:2] # top 2 variable correlated to PC1
varPC.sig<-cbind(df[,c(order(abs(varPC1Corr), decreasing = T)[1:2])],PC1=PC1) 
ggpairs(data=varPC.sig)
plot(varPC1Corr[order(abs(varPC1Corr), decreasing = T)[1:10]],main="Top 10 variable correlated to PC1") # top 10 variable correlated to PC1

## Use 75% of the sample as training set
smp_size <- floor(0.75 * nrow(df))
set.seed(123)
train_ind <- sample(seq_len(nrow(df)), size = smp_size)
train <- df[train_ind, ]
test <- df[-train_ind, ]

# Separate variables and class
x<-subset(train,select=-class)
y<-train[,"class"]
x.test<-subset(test,select=-class)
y.test<-test[,"class"]

# Classifier 1: SVM
# SVM is good for when # of variables > # of records
svm_model <- svm(class ~., data=train, type="C-classification",cross=10)
svm_test<-predict(svm_model,x.test)
table(svm_test, y.test)
mean(svm_test == y.test)
# Tune SVM
svm_tune <- tune(svm, train.x=x, train.y=y, kernel="radial", ranges=list(cost=10^(-1:2), gamma=c(.5,1,2)))
# Using the best cost and gamma to build SVM again
svm_model_tuned<-svm(class ~., data=train, type="C-classification", kernel="radial", cost=svm_tune$best.parameters$cost,gamma=svm_tune$best.parameters$gamma, cross=10)

# Order by the class correlation
var.sort<-order(abs(varClassCorr), decreasing = T)
best.performance=1
svm_best_tune = svm_tune
best.trainSet = train
for (v in 1:10){
  trainSet<- train[,var.sort[1:v]]
  svm_tune <- tune(svm, train.x=trainSet, train.y=y, kernel="radial", ranges=list(cost=10^(-1:2), gamma=c(.5,1,2)))
  if(svm_tune$best.performance < best.performance){
    best.performance = svm_tune$best.performance
    svm_best_tune = svm_tune
    best.trainSet = trainSet
  } 
}
head(best.trainSet)
svm_best_tune$best.performance
svm_best_model<-svm(class ~., data=cbind(best.trainSet, class=y), type="C-classification", kernel="radial", cost=svm_best_tune$best.parameters$cost,gamma=svm_best_tune$best.parameters$gamma, cross=10)


summary(svm_model)
summary(svm_model_tuned)
summary(svm_best_model)

svm_tuned_test<-predict(svm_model_tuned,x.test)
svm_best_model_test<-predict(svm_best_model,x.test)
table(svm_best_model_test, y.test)
mean(svm_best_model_test == y.test)

print(svm_tune)
table(svm_tuned_test, y.test)
mean(svm_tuned_test == y.test)

# Classifier 2: Random forest
# Random forest is good at classifier with strong non-linearity.
rf_model <- randomForest(as.factor(class) ~ ., data=train, importance=TRUE, ntree=2000)
plot(rf_model)

print(rf_model)
pred_rf<-predict(rf_model,x)
table(pred_rf,y) # type I & II
mean(pred_rf == y) # RF training error

rf_model_test<-predict(rf_model,x.test)
table(rf_model_test, y.test) # test type I & II
mean(rf_model_test == y.test) # RF test error









