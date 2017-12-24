library(RSQLite)
library(DBI)
library(ggplot2)
library(dplyr)
library(maps)
library(ggmap)
library(devtools)
library(gganimate)

con <- dbConnect(RSQLite::SQLite(), "~/workspace/github/mobike-crawler/temp.db")
dbListTables(con)
raw<-dbReadTable(con, "mobike", row.names = FALSE)
dbDisconnect(con)

df<- raw %>%
  select(runID, Time, distId, bikeType, lon, lat) %>%
  group_by(runID, distId) %>%
  summarize(bikeType = mean(bikeType), lon = mean(lon), lat= mean(lat))

head(df)
df$runID<-as.factor(df$runID)

map <- get_map(location =  c(lon = mean(df$lon), lat =	mean(df$lat)), maptype = "roadmap", zoom = 15)
#ggmap(map, extent = "device") + 
#  geom_point(aes(x=lon, y=lat), colour="red", alpha=0.05, size=0.2, data=df)

ggmap(map, extent = "device") + 
  geom_density2d(data=df, aes(x = lon, y = lat), size = 0.2)+
  stat_density2d(data = df, 
                 aes(x = lon, y = lat, fill = ..level.., alpha = ..level..), size = 0.01, 
                 bins = 16, geom = "polygon") + 
  scale_fill_gradient(low = "green", high = "red") + 
  scale_alpha(range = c(0, 0.3), guide = FALSE)

p<-ggmap(map, extent = "device") + 
  geom_density2d(data=df, aes(x = lon, y = lat, frame=runID), size = 0.2)+
  stat_density2d(data = df, 
                 aes(x = lon, y = lat, fill = ..level.., alpha = ..level.., frame=runID), size = 0.01, 
                 bins = 16, geom = "polygon") + 
  scale_fill_gradient(low = "white", high = "red") + 
  scale_alpha(range = c(0, 0.3), guide = FALSE)
p
setwd("~/workspace/github/datalab/mobikeData/")
gganimate(p)
write.gi
write.csv(df,"~/workspace/github/datalab/mobikeData/wuhan_snapshot_11_2_2017.csv",row.names = F)
