library('rjson')
# access json data from OpenFDA database https://open.fda.gov/api/reference/
# https://open.fda.gov/drug/event/reference/
# The query is about adverse drug event reports since 2004
json_file<-'https://api.fda.gov/drug/event.json?api_key=htijXuyJCPIjFsjB9AEa3S4lbQkPZ3uX5XXBsaD2&search=receivedate:[20040101+TO+20150101]&count=receivedate'
json_data<-fromJSON(file=json_file)
df<-as.data.frame(do.call(rbind.data.frame,json_data$result))
df['time']<-as.Date(df$time,'%Y%m%d')
df['year']<-as.numeric(substring(df$time, 1,4))
df['month']<-as.numeric(substring(df$time, 5,6))
df['day']<-as.numeric(substring(df$time, 7,8))

library(ggplot2)
library(scales)
library(ts)

df.1 <- filter(df$count,filter=rep(1/5,5))
df.2 <- filter(df$count,filter=rep(1/25,25))
df.3 <- filter(df$count,filter=rep(1/81,81))
df.4 <- filter(df$count,filter=rep(1/150,150))
plot(df.1,col="grey")
lines(df.2,col="red")
lines(df.3,col="blue")

p<-ggplot(df,aes(x=time, y=count))
p +
  geom_line(colour="grey")+
  scale_x_date(labels=date_format("%b-%Y")) + xlab("")+
  geom_line(aes(y=df.2),colour="red")+
  geom_line(aes(y=df.3),colour="blue")+
  geom_line(aes(y=df.4),colour="black")