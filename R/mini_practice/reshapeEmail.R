library(reshape)
library(dplyr)
N=20
# Generate N random id between 1 and 10
id<- sample(10,N, replace = T)
# Generate N random email like crap
email<- paste(stringi::stri_rand_strings(N, 4),'@',stringi::stri_rand_strings(N, 3), '.com', sep='')
# the dataframe
df<-data.frame(id, email)
head(df)

df<-df %>%
    group_by(id) %>%
    mutate(rank = row_number()) %>%
    as.data.frame()
#df$rank<-as.factor(df$rank)
head(df)
df_cast<-cast(df, id~rank, value='email')
# Add prefix of the 1, 2, 3, 4
#colnames(df_cast)[-1]<-paste('email_',colnames(df_cast)[-1])
df_cast
