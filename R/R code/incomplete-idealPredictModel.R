library(ggplot2)
library(FNN)

raw<- read.csv('C:\\Users\\edward.wang\\Documents\\Project_SWAT\\RepSurvey\\data\\idealCustomerFactor_group_reduced.csv')



df<-raw[,13:ncol(raw)]
## 75% of the sample size
smp_size <- floor(1 * nrow(df))
## set the seed to make your partition reproductible
set.seed(123)
train_ind <- sample(seq_len(nrow(df)), size = smp_size)

train<- df[train_ind,]
test<-df[-train_ind,]
x_train<-train[,2:ncol(train)]
y_train<-raw[,10:12]

#******* Test the correlation among factors******* 
# Test colinearity among factors. Consider to reduce the dimension if it is the case.
# **No use in production code
#*************************************************
library(GGally)
#******* Test the correlation among factors******* 
# Test colinearity among factors. Consider to reduce the dimension if it is the case.
# **No use in production code
#*************************************************
ggcorr(x_train[,c("Q16postacute","Q16providerAndPayer","Q16employment","Q4", "IDN_beds_INT")], nbreaks = 5, hjust=0.75, size=3, color='grey50',
       label = TRUE, label_size = 4, label_round = 2, label_alpha = TRUE)
ggcorr(y_train, nbreaks = 10, hjust=0.75, size=4, color='grey50',
       label = TRUE, label_size = 5, label_round = 2, label_alpha = F)
z <- cor(x_train)
z[lower.tri(z,diag=TRUE)] <- 0
zdf <- as.data.frame(as.table(z))
zdf <- subset(zdf, Var1!=Var2)

marketSegment<-function(raw, surveyOnly=T, K=50){
  set.seed(100)
  df<-raw[,13:ncol(raw)]
  ## 75% of the sample size
  smp_size <- floor(1 * nrow(df))
  ## set the seed to make your partition reproductible
  set.seed(123)
  train_ind <- sample(seq_len(nrow(df)), size = smp_size)
  
  train<- df[train_ind,]
  test<-df[-train_ind,]
  x_train<-train[,2:ncol(train)]
  y_train<-raw[,10:12]
  
  #******* Ideal predictive model******* 
  # Use all factors to predict ideal and get the importance score as the weights for segmentation.
  # Random forest is used as the learning model
  #*************************************************
  library(randomForest)
  fmla <- as.formula(paste("ideal ~ ", paste(colnames(train)[-1], collapse= "+")))
  train$ideal<-as.factor(train$ideal)
  fit.rf <- randomForest(fmla,data=train)
  print(fit.rf)
  
  # Turn factor importance to df format.
  importanceOrder=order(-fit.rf$importance)
  importantce.df<-data.frame('var' = names(fit.rf$importance[importanceOrder,]), 'importance'=fit.rf$importance[importanceOrder,])
  
  # Probability to be "ideal" [0,1]
  rf.p<-predict(fit.rf, df[,-1], type='prob')
  # Class of "ideal" 0/1
  rf.c<-as.numeric(as.character(predict(fit.rf, df[,-1])))
  
  output<-raw
  # rm weight
  weight<-fit.rf$importance
  weight<-c(5,5,5,5,0,0,0,0,2,0,2,2,5,5,4,3,4,0,5)
  
  output[, 14:ncol(output)]<-apply(output[, 14:ncol(output)], 2, function(x){
    return ((x-min(x))/(max(x)-min(x)+0.000001))
  })
  
  #output[, 14:ncol(output)]<-output[, 14:ncol(output)]*weight
  output[, 14:ncol(output)]<-mapply('*',output[, 14:ncol(output)],weight)
  #head(mapply('*',output[1:2, 14:ncol(output)],weight))
  output<-subset(output, (Q1CLI+Q1ECO)>0 & (Q11CLI+Q11ECO)>0 & (Q12CLI+Q12ECO)>0)
  
  df.seg<-output[, c("CLI", "ECO","INT","REL","ideal", "soldToNumber")]
  df.seg$CLI<- output$Q12CLI + output$Q11CLI+ output$Q10CLI + output$Q13CLI+ output$Q1CLI
  df.seg$ECO<- output$Q12ECO + output$Q11ECO+ output$Q10ECO + output$Q13ECO+ output$Q1ECO + output$Q3
  
  df.seg$REL<- output$Q10REL + output$Q9
  if(surveyOnly){
    df.seg$INT<- output$Q10INT + output$Q16postacute + output$Q16providerAndPayer + output$Q16employment + output$Q4
  }else{
    df.seg$INT<- output$Q10INT + output$Q16postacute + output$Q16providerAndPayer + output$Q16employment + output$Q4 +output$IDN_beds_INT
  }
  
  #df.seg<-subset(df.seg, CLI+ECO+REL>0)
  summary(df.seg[,1:3])
  # Normalize each column
  df.seg[,1:3]<-apply(df.seg[,1:3], 2, function(x){
    return ((x-min(x))/(max(x)-min(x)+0.000001))
  })
  
  # Normalize the three buckets
  df.seg[,1:3]<-t(apply(df.seg[,1:3], 1, function(x){
    x/(sum(x)+0.000001)
  }))
  
  # Using row-column-normalized value as clustering data
  df.cluster<-df.seg[,1:3]
  km <- kmeans(df.cluster, 4, nstart = 20)
  
  # Calculate posterier by using K nearest neighbor
  kn<-get.knn(df.cluster, k=K)
  posterior_knn<-apply(kn[[1]], 1, function(x){
    table(as.factor(km$cluster)[x])/K
  })
  posterior_knn<-t(posterior_knn)
  
  #Create a vector for storing the sse
  sse=vector('numeric')
  for(i in 2:10){
    #k-means function in R has a feature withinss which stores sse for each cluster group
    sse[i-1]=sum(kmeans(df.cluster,centers=i)$withinss)
  }
  #Converting the sse to a data frame and storing corresponding value of k
  sse=as.data.frame(sse)
  sse$k=seq.int(2,10)
  
  library(cluster) 
  
  x<-output[which(output$soldToNumber %in% df.seg$soldToNumber),]
  x$km<-km$cluster
  output<-x
  output[,c("CLI", "ECO", "INT","REL")]<-df.seg[,1:4]
  output<-cbind(output, posterior_knn)

  return(list(output, weight))
}
attach(raw)

output_all<-marketSegment(raw, surveyOnly=F)
weight_all<-output_all[[2]]
table(output_all[[1]]$km)/sum(table(output_all[[1]]$km))
detach(raw)
result<-output_all[[1]]
# Ouput the Segmented result
write.csv(result,'A:\\Teams\\SWAT\\Survey and Segmentation\\Rep survey and segmentation\\data\\allCustomersIdealModel.csv', row.names = F)

#************Shrink Factor******************
var<-c('Q1CLI',
            'Q1ECO',
            'Q3',
            'Q4',
            'Q9',
            'Q10CLI',
            'Q10ECO',
            'Q10INT',
            'Q10REL',
            'Q11CLI',
            'Q11ECO',
            'Q12CLI',
            'Q12ECO',
            'Q13CLI',
            'Q13ECO',
            'Q16employment',
            'Q16postacute',
            'Q16providerAndPayer',
            'IDN_beds_INT'
            
)
var.question<-c(1,1,3,4,9,10,10,10,10,11,11,12,12,13,13,16.1,16.2,16.3,17)
var.group<-c(1,1,2,3,4,5,5,5,5,6,6,7,7,8,8,9.1,9.2,9.3,10)
var.df<-data.frame(var, var.question, var.group)



q<-unique(var.question)
output<-output_all[[1]]
output$km<-as.factor(output$km)
log<-data.frame(step=NULL, question=NULL, error=NULL)
removalList<-NULL

df.out<-output[, c("CLI", "ECO","INT", "km")]
df.out$CLI<- output$Q12CLI + output$Q11CLI+ output$Q10CLI + output$Q13CLI+ output$Q1CLI
df.out$ECO<- output$Q12ECO + output$Q11ECO+ output$Q10ECO + output$Q13ECO+ output$Q1ECO + output$Q3
df.out$INT<- output$Q10INT + output$Q16postacute + output$Q16providerAndPayer + output$Q16employment + output$Q4 +output$IDN_beds_INT

# Normalize each column
df.out[,1:3]<-apply(df.out[,1:3], 2, function(x){
  return ((x-min(x))/(max(x)-min(x)+0.000001))
})
# Normalize the three buckets
df.out[,1:3]<-t(apply(df.out[,1:3], 1, function(x){
  x/sum(x)
}))

# Using rf to predict segment based on full survey
rf<-randomForest(km~.,data=df.out[complete.cases(df.out),], ntree=200)
print(rf)
errRate<-rf$err.rate[nrow(rf$err.rate),'OOB']
log<-rbind(log, list(0, 0, errRate))

# Using LDA to predict the segment
library(MASS)
library(matlib)
library(mvtnorm)
require(scales)
df.out<-read.csv("A:\\Teams\\SWAT\\Survey and Segmentation\\Rep survey and segmentation\\data\\FinalizedSegments_12-28.csv")
df.out<-na.omit(df.out)
df.out<-df.out[,c(2,3,4,5)]
names(df.out)[4]<-"km"
train<-df.out[complete.cases(df.out),]
lda<-lda(km~.,data=train, CV=F)
ldaProjection <- as.data.frame(cbind(as.matrix(train[,-4]) %*% lda$scaling,km=train$km))
ldaProjection$km<-as.factor(ldaProjection$km)
x<-as.matrix(train[,1:2])
sigma<-cov(x)
u<-lda$means[,1:2]
pi<-lda$prior
nx<-c(1,2,3)

coef<-apply(u,1, function(x){
  return (solve(sigma)%*%x)
})
const<-as.matrix(apply(u, 1, function(x){
  return(-0.5*x%*%solve(sigma)%*%x)
})+log(pi))


apply(u,1, function(x){
  return (solve(sigma)%*%x)
})

as.matrix(apply(u, 1, function(x){
  return(-0.5*t(x)%*%solve(sigma)%*%x)
})+log(pi))
coef
const
# Assess the accuracy of the prediction
# percent correct for each category of km
lda<-lda(km~.,data=train, CV=T)
ct <- table(df.out[complete.cases(df.out),]$km, lda$class)
diag(prop.table(ct, 1))
# total percent correct by using CV
sum(diag(prop.table(ct)))
df.out$lda <- lda$class

# Use logistic regression to predict the segment
# rf > LDA > Logistic
library(boot)
logi<-glm(km~.,data=df.out[complete.cases(df.out),], family = binomial(logit))
cv.logi<-cv.glm(df.out[complete.cases(df.out),], logi)
km.predicted <- logi$fitted
km.diag <- glm.diag(logi)
# Error from CV
mean((logi$y-km.predicted)^2/(1-km.diag$h)^2)








