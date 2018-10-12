library(igraph)

# Mehotd 3: use plot to draw network
##
xcor <- runif(100, min=1, max=100)
ycor <- runif(100, min = 1, max = 100)
weight<-runif(100, min=0)

dfpt<-data.frame(xcor, ycor, weight)
symbols(x=xcor, y=ycor, circles=weight, inches=0.25,ann=F, bg="steelblue2")

mx <- mean(xcor*weight)/mean(weight)
my <- mean(ycor*weight)/mean(weight)


g<-graph.edgelist(cbind(rep(1,100),2:101)) # 1: NLC, 2~11: DC
l<-cbind(c(mx,xcor),c(my,ycor))
E(g)$width <- weight*5
E(g)$arrow.width <- Edges$width * 5
E(g)$curved <- 0
plot(g,layout=l, vertex.size=c(20, weight*10))


