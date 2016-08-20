library(geosphere)
library(igraph)
library(statnet)
# Prep stores data
storesRaw<-read.csv("E:\\my work\\python code\\stores.csv",sep="\t")
colnames(storesRaw)<-c("id","lat","lon")
storesRaw$id<-as.character(storesRaw$id)
storesRaw$st<-5 # serivce time at store is 5 min
storesRaw$frm<-"49"
speed<-5 # 5 miles per min

startTime<-as.POSIXct("2014-03-13 15:00:00")
# Prep deliveries data
deliveriesRaw<-read.csv("E:\\my work\\python code\\deliveries.csv",sep="\t")
colnames(deliveriesRaw)<-c("id","due","lat","lon","qty")
deliveriesRaw$id<-as.character(deliveriesRaw$id)
deliveriesRaw$st<-deliveriesRaw$qty
deliveriesRaw$due<-as.POSIXct(deliveriesRaw$due)

nodeList<-rbind(deliveriesRaw[,c("id","lon","lat","st")], storesRaw[,c("id","lon","lat", "st")])
customers<-deliveriesRaw[,c("id","st","due")]
rownames(nodeList)<-nodeList$id


trip = list()
# Generate store distance matrix from lon/lat
nodeDist<-distm(as.matrix(nodeList[,c("lon","lat")])) # Completed distance matrix
colnames(nodeDist)<-row.names(nodeList) 
rownames(nodeDist)<-row.names(nodeList)

storeDist<-distm(as.matrix(storesRaw[,c("lon","lat")])) # store distance matrix
customerDist<-distm(as.matrix(deliveriesRaw[,c("lon","lat")])) # customer distance matrix

# Step 1: Assign customers to its nearest store
customerToStoreDist<-distm(as.matrix(deliveriesRaw[,c("lon","lat")]) ,as.matrix(storesRaw[,c("lon","lat")]))
row.names(customerToStoreDist)<-deliveriesRaw$id
colnames(customerToStoreDist)<-storesRaw$id

# Look for the nearest store name for each customer
customerToStorePair <- t(sapply(seq(nrow(customerToStoreDist)),function(i) {
  j <- which.min(customerToStoreDist[i,])
  c(colnames(customerToStoreDist)[j], customerToStoreDist[i,j])
}))

colnames(customerToStorePair)<-c("store", "distance")
row.names(customerToStorePair)<-deliveriesRaw$id
customerToStorePair<-as.data.frame(customerToStorePair)
customerToStorePair$store<-as.character(customerToStorePair$store)
customerToStorePair$customer<-rownames(customerToStorePair)
customerToStoreDist$id<-rownames(customerToStorePair)
rownames(customers)<-customers$id
str(customerToStorePair)
customers$frm<-customerToStorePair$store
customers$at<-startTime+nodeDist[customers$id,customerToStorePair$store]/speed*60
obj<-sum(abs(customers$at-customers$due))


G<-graph.data.frame(rbind(customers[,c("frm","id")], storesRaw[,c("frm","id")]))
plot(G)
V(G)$st<-nodeList[V(G)$name,]$st
V(G)$lon<-nodeList[V(G)$name,]$lon
V(G)$lat<-nodeList[V(G)$name,]$lat


for (c in customerToStorePair$customer){
  newTrip<-list(customerToStorePair[c,]$store, c)
  trip<-append(trip, newTrip)
}
trip[[1]]

updateNetwork<-function(trip){
  return customers, edges
}
arrivingTime<-function(trip)
# Step 2: Generate initial route by saving algorithm



# Step 3: Adaptive Large Neighborhood Search (ALNS)

# Step 4: Tabu search

# Print out solution