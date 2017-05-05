type<-c("missing", "done","not_apply")
index<-sample(1:3, 10,replace = T)
type[index]
# Generate a df with 5 col which contains missing/done/not_apply randomly
df<-data.frame(A=type[sample(1:3, 10,replace = T)], 
               B=type[sample(1:3, 10,replace = T)],
               C=type[sample(1:3, 10,replace = T)],
               D=type[sample(1:3, 10,replace = T)],
               E=type[sample(1:3, 10,replace = T)])
# Take a look of the dummy df
head(df)

# Apply along each row
df$missing<-apply(df, 1, function(x){
  # get the col names of the vector x, if the element of x has value == "missing"
  paste(names(x[which(x=="missing")]), collapse = ', ')
})
df