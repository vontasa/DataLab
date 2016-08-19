library(reshape2)
library(ggplot2)
library(dplyr)
raw<-read.csv("C:\\Users\\edward.wang\\Documents\\GitHub\\adsdc\\dataset_challenge_one.tsv",sep="\t", header=TRUE)
str(raw)
summary(raw)
mcor<-cor(raw)

melt_df<-melt(mcor)
melt_df<-melt_df[order(melt_df$value),]

# Check the correlation between attributes
p_histogram_correlation<-ggplot(melt_df, aes(x=value)) + 
  geom_histogram()+
  labs(x = "Correlation")+
  ggtitle("Distribution of Attribute Correlation")
p_histogram_correlation
# The mean is close to 0 and like bell shape which means the selected attributes are not heavily correlated.
summary(melt_df) # 6210 NA value, not many
normaliy<-sapply(raw, function(x) shapiro.test(x)$p.value)
normaliy<-normaliy[order(normaliy,decreasing = T)]
plot(normaliy)
p_variable_normality<-ggplot(normaliy) + geom_bar()
p_variable_normality
names(normaliy[normaliy>=0.05])
