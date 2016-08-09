require(geosphere)
require(igraph)
require(statnet)
deliveriesRaw<-read.csv("C:\\Users\\edward.wang\\Documents\\R code\\MDVRTWP\\deliveries.csv",sep="")
storesRaw<-read.csv("C:\\Users\\edward.wang\\Documents\\R code\\MDVRTWP\\stores.csv",sep="")
row.names(storesRaw)<-storesRaw$store_id
nodeList<-rbind(deliveriesRaw[,c("longitude","latitude")], storesRaw[,c("longitude","latitude")])
head(deliveriesRaw)
head(storesRaw)

# Generate store distance matrix from lon/lat
nodeDist<-distm(as.matrix(nodeList[,c("longitude","latitude")])) # Completed distance matrix
colnames(nodeDist)<-row.names(nodeList) 
row.names(nodeDist)<-row.names(nodeList)
storeDist<-distm(as.matrix(storesRaw[,c("longitude","latitude")])) # store distance matrix
customerDist<-distm(as.matrix(deliveriesRaw[,c("longitude","latitude")])) # customer distance matrix

# Step 1: Assign customers to its nearest store
customerToStoreDist<-distm(as.matrix(deliveriesRaw[,c("longitude","latitude")]) ,as.matrix(storesRaw[,c("longitude","latitude")]))
row.names(customerToStoreDist)<-row.names(deliveriesRaw)
colnames(customerToStoreDist)<-storesRaw$store_id

# Look for the nearest store name for each customer
customerToStorePair <- t(sapply(seq(nrow(customerToStoreDist)),function(i) {
  j <- which.min(customerToStoreDist[i,])
  c(colnames(customerToStoreDist)[j], customerToStoreDist[i,j])
}))
colnames(customerToStorePair)<-c("store", "distance")
row.names(customerToStorePair)<-row.names(deliveriesRaw)
customerToStorePair<-as.data.frame(customerToStorePair)


# Step 2: Generate initial route by saving algorithm

for (s in storesRaw$store_id){ # For each store group, generate initial route
  temp<-subset(customerToStorePair,store==s)
}

# Step 3: Adaptive Large Neighborhood Search (ALNS)

# Step 4: Tabu search

# Print out solution