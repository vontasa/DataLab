# -----------------------------------------------------------------------------
# Shipping co route planning model
# Yan "Edward" Wang
# Date: 10/16/2017
# Description:
# Given the historical shipment information, plan the route to reduce the operational cost and estimate the # of vessels required
# -----------------------------------------------------------------------------

require(igraph)
require(readxl)
# Edge attributes
network.raw<-read_xlsx('~/workspace/BCG challenge/Ops Case - ShippingCo_dataset2.xlsx', sheet='network')
# Vertices attributes
port.raw<-read_xlsx('~/workspace/BCG challenge/Ops Case - ShippingCo_dataset2.xlsx', sheet='Ports')
# Routes
routes.raw<-read_xlsx('~/workspace/BCG challenge/Ops Case - ShippingCo_dataset2.xlsx', sheet='Routes')
graph<-graph_from_data_frame(d=network.raw, vertices=port.raw, directed=T)
# Remove isolated vertices
graph<-delete.vertices(graph, degree(graph)==0)
# Define line color
colrs <- c("darkgoldenrod3", "tomato", "deepskyblue3")
E(graph)$color <- colrs[as.factor(E(graph)$line)]

# Fix the seed and network topology.
set.seed(13)
l <- layout_with_fr(graph)

# Take a look at the network
plot(graph, layout=l,
     edge.arrow.size=1,
     edge.width=1,
     vertex.label.cex=0.75, 
     vertex.label.family="Helvetica",
     vertex.label.font=2,
     vertex.shape="circle", 
     vertex.size=0, 
     vertex.label.color="black")
legend(x=1, y=1, unique(E(graph)$line), pch=21, pt.bg=levels(as.factor(E(graph)$color)), pt.cex=2, cex=.8, bty="n", ncol=1)

# Return the cost of a given path and the transshipment port
# return a list: result[cost = cost, trans = transshipment port]
pathCost<-function(g, p){
  # Loading, discharge and shipping cost
  routeCost<-sum(E(g,path=p)$cost)
  loadCost<-(V(g)[head(p,n=1)$name]$load)
  dischargeCost<-V(g)[tail(p,n=1)$name]$discharge
  
  # Line of each edge in the path
  line<-as.factor(E(g,path=p)$line)
  # Index of transshipment port in the given path
  trans.index<-1+which(diff(as.integer(line))!=0)
  trans.v<-p[trans.index]$name
  transshipCost<-sum(V(g)[name%in%trans.v]$transship)
  result<-list("cost"=transshipCost+routeCost+loadCost+dischargeCost, "trans"=trans.v)
  return(result)
}
# cabotage rule is not used in this version
cabotage<-c("BR")

# Initialize the output df
output<-data.frame(id=numeric(), from=character(), to=character(), path=character(), cost=numeric(), line1=character(), line2=character(), line3=character(), trans1=character(), trans2=character(), legs=character(),beforeCost=numeric(), beforeLegs=numeric(),stringsAsFactors=FALSE)
# Initialize the network flow
E(graph)$currentVolume<-0
E(graph)$currentWeight<-0
# MAIN LOOP: 
# Minimize shipping cost: Given from/to, output the route with lowest cost for each container.
for(i in 1:nrow(routes.raw)){ #1:nrow(routes.raw)
  container<-routes.raw[i,]
  # Get all the simple paths from loading port to discharge port
  path.all<-all_simple_paths(graph, 
                             from = V(graph)[name==container$`Port of Load`],
                             to  = V(graph)[name==container$`Port of Discharge`])
  
  # Given a path, calculate the cost
  result<-sapply(path.all, FUN=pathCost, g=graph)
  pathCost.all<-unlist(result[1,])
  # get the index of the path with the lowest cost
  index.best<-which.min(pathCost.all)
  path.best<-path.all[[index.best]]
  edge.best<-E(graph,path=path.best)
  
  # Update the network flow (weight flow and volume flow)
  E(graph,path=path.best)$currentVolume<-E(graph,path=path.best)$currentVolume+container$Volume
  E(graph,path=path.best)$currentWeight<-E(graph,path=path.best)$currentWeight+container$Weight

  # Transshipment port for the best route
  trans.best<-unlist(result[2,index.best])
  # Format the path as one string
  path.best.str<-paste(path.best$name, collapse = ", ")
  lines<-unique(E(graph,path=path.best)$line)
  # Format the output for container i
  record<-list(id=routes.raw[i,]$`Container ID`, from=path.best[1]$name, to=path.best[length(path.best)]$name, path=path.best.str, cost=min(pathCost.all), 
               line1=lines[1], line2=lines[2], line3=lines[3], trans1=trans.best[1], trans2=trans.best[2], legs=length(E(graph,path=path.best)),beforeCost=0, beforeLegs=0)
  record<-as.data.frame(record, stringsAsFactors = F)
  # Update the output df
  output<-rbind.data.frame(output, record)
}
# Append some additional information to the output df
output$volume<-routes.raw$Volume
output$weight<-routes.raw$Weight


# List of subgraph for each line
lineSubgraph<-lapply(unique(network.raw$line), function(x){
  return(subgraph.edges(graph, E(graph)[line==x]))
})
names(lineSubgraph)<-unique(network.raw$line)


# Evaluate the cost of historical records;
# validate the data and find out suspecious records
# If there is error, return cost and legs as 1 and error code
# Error code is defiend as below:
# 100: loading port error
# 10: transshipment port error
# 1: discharge port error

before<-apply(routes.raw, 1, function(x){
  legs<--1
  cost<--1
  # indicator of valid transshipment port
  t_flag<-T
  # indicator of valid discharge port
  d_flag<-T
  if(is.na(x['First Service'])){
  # indicator of valid loading port
    l_flag<-F
    return(c(err=(!l_flag)*100+(!t_flag)*10+(!d_flag)*1, cost=cost, legs=legs))
  }
  l_flag<-x['Port of Load']%in%V(lineSubgraph[[x['First Service']]])$name
  
  # If NO transshipment
  if(is.na(x['Port of Transhipment'])){
    g<-lineSubgraph[[x['First Service']]]
    d_flag<-x['Port of Discharge']%in%V(g)$name
    if(d_flag){
      p<-shortest_paths(g, output = "both",
                         from = V(g)[name==x['Port of Load']],
                         to  = V(g)[name==x['Port of Discharge']])
      cost<-sum(E(g,path=p$vpath[[1]])$cost)+V(g)[x['Port of Load']]$load+V(g)[x['Port of Discharge']]$discharge
      legs<-length(E(g,path=p$vpath[[1]]))
    }
  # Transshipment
  }else{
    # g1=graph of first service, g2=graph of the second service
    g1<-lineSubgraph[[x['First Service']]]
    g2<-lineSubgraph[[x['Second Service']]]
    d_flag<-x['Port of Discharge']%in%V(g2)$name
    t_flag<-x['Port of Transhipment']%in%V(g2)$name && x['Port of Transhipment']%in%V(g1)$name
    if(d_flag&&t_flag){
      # Shortest path from loading port to transshipment
      p1<-shortest_paths(g1, output = "both",
                        from = V(g1)[name==x['Port of Load']],
                        to  = V(g1)[name==x['Port of Transhipment']])
      cost1<-sum(E(g1,path=p1$vpath[[1]])$cost)
      legs1<-length(E(g1,path=p1$vpath[[1]]))
      
      # Shortest path from transshipment to discharge port
      p2<-shortest_paths(g2, output = "both",
                         from = V(g2)[name==x['Port of Transhipment']],
                         to  = V(g2)[name==x['Port of Discharge']])
      cost2<-sum(E(g2,path=p2$vpath[[1]])$cost)
      legs2<-length(E(g2,path=p2$vpath[[1]]))
      
      # Total cost and total number of legs
      cost<-cost1+cost2+V(g1)[x['Port of Load']]$load+V(g2)[x['Port of Discharge']]$discharge+V(g2)[x['Port of Transhipment']]$transship
      legs<-legs1+legs2
    }
  }
  return(c(err=(!l_flag)*100+(!t_flag)*10+(!d_flag)*1, cost=cost, legs=legs))
})

output$beforeErr<-before['err',]
output$beforeCost<-before['cost',]
output$beforeLegs<-before['legs',]
# calculate the number of vessels needed by each edge
vessels<-max(ceiling(E(graph)$currentWeight/E(graph)$wlimit), ceiling(E(graph)$currentVolume/E(graph)$vlimit))
network.raw$currentVolume<-E(graph)$currentVolume
network.raw$currentWeight<-E(graph)$currentWeight
network.raw$vessels<-pmax(ceiling(network.raw$currentVolume/network.raw$vlimit),ceiling(network.raw$currentVolume/network.raw$vlimit))
# calculate the vessel utilization by each edge
network.raw$volumeUtil<-network.raw$currentVolume/(network.raw$vessels*network.raw$vlimit)
network.raw$weightUtil<-network.raw$currentWeight/(network.raw$vessels*network.raw$wlimit)
# Weight flow
plot(graph, layout=l,
     edge.arrow.size=1,
     edge.width=E(graph)$currentWeight/50,
     edge.label = E(graph)$currentWeight,
     edge.label.family="Helvetica",
     edge.label.font=2,
     edge.label.cex= 1,
     
     vertex.label.cex=1.2, 
     vertex.label.family="Helvetica",
     vertex.label.font=2,

     vertex.shape="circle", 
     vertex.size=0, 
     vertex.label.color="black")
legend(x=1, y=1, unique(E(graph)$line), pch=21, pt.bg=levels(as.factor(E(graph)$color)), pt.cex=2, cex=.8, bty="n", ncol=1)

# volume flow
plot(graph, layout=l,
     edge.arrow.size=1,
     edge.width=E(graph)$currentVolume/5,
     edge.label = E(graph)$currentVolume,
     edge.label.family="Helvetica",
     edge.label.font=2,
     edge.label.cex= 1,
     
     vertex.label.cex=1.2, 
     vertex.label.family="Helvetica",
     vertex.label.font=2,
     vertex.shape="circle", 
     vertex.size=0, 
     vertex.label.color="black")
legend(x=1, y=1, unique(E(graph)$line), pch=21, pt.bg=levels(as.factor(E(graph)$color)), pt.cex=2, cex=.8, bty="n", ncol=1)

# Main output, contains all routes
write.csv(output, '~/workspace/BCG challenge/routes2.csv',row.names=FALSE)
# Number of vessels required by each edge. Use this info to estimate the vessel utilization
write.csv(network.raw, '~/workspace/BCG challenge/network2.csv',row.names=FALSE)
