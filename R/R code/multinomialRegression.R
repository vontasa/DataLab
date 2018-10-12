# Multinomial logistic regression also known as softmax regression is a regression model which
# generalizes logistic regression by allowing more than two discrete outcome

require(foreign)
require(nnet)
require(ggplot2)
require(reshape2)
ml<-read.dta("http://www.ats.ucla.edu/stat/data/hsbdemo.dta")
with(ml, table(ses, prog))
with(ml, do.call(rbind, tapply(write, prog, function(x) c(M = mean(x), SD = sd(x)))))
ml$prog2 <- relevel(ml$prog, ref = "academic")
test <- multinom(prog2 ~ ses + write, data = ml)
summary(test)
#Calaulate the p-value for each coefficients since multinom doesn't include the p-value
z <- summary(test)$coefficients/summary(test)$standard.errors
p <- (1 - pnorm(abs(z), 0, 1)) * 2
p

head(pp <- fitted(test))
dses <- data.frame(ses = c("low", "middle", "high"), write = mean(ml$write))
predict(test, newdata = dses, "probs")
dwrite <- data.frame(ses = rep(c("low", "middle", "high"), each = 41), write = rep(c(30:70),
                                                                                   3))
## store the predicted probabilities for each value of ses and write
pp.write <- cbind(dwrite, predict(test, newdata = dwrite, type = "probs", se = TRUE))
## calculate the mean probabilities within each level of ses
by(pp.write[, 3:5], pp.write$ses, colMeans)

lpp <- melt(pp.write, id.vars = c("ses", "write"), value.name = "probability")
head(lpp)  # view first few rows

ggplot(lpp, aes(x = write, y = probability, colour = ses)) + geom_line() + facet_grid(variable ~
  ., scales = "free")
