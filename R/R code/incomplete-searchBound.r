# ######################################################
# Boundary search
# Yan.W
# Description: 
# Search the optimal upper/lower percentile bound of customer group
# #######################################################
library(dplyr)
#raw<-read.csv('A:\\Teams\\SWAT\\Market Intelligence (GHX)\\data\\quarter sales data-v2.csv')
raw<-read.csv('A:\\Teams\\SWAT\\Market Intelligence (GHX)\\data\\quarter sales data-TAM version.csv')

# Create a small sample for dev test
"
names <- unique(raw$SoldToCustomerId)
sample_index <- sample(1:length(names), 0.1 * length(names))
sample_names<-names[sample_index]
raw<-filter(raw, SoldToCustomerId %in% sample_names)
"

# Function to return the starting point of search
# Starting at median, starting at peak, starting at avg
SearchStart <- function(df, method = 'median', target = NULL){
  solution <- switch(method,
    median = round(nrow(df)/2, 0),
    target = which.min(abs(df$slope - target)) 
  )
  return(solution)
}

# Loss function (minimimun obj function) for seaching
# Resturn the value of performance
# est: estimated value; actual: actual value
# return standard error
Loss <- function(est, actual){
  se <- mean((est - actual)^2)^0.5
  return(se)
}

# Total loss from all channel-category
Loss_total <- function(cc.list, df, ghx){
  loss_total <- 0 
  for (c in cc.list){
    # Normalized estimation
    est <- df %>% group_by(id, cc) %>%
      mutate(qty.norm = qty/mean(qty)) %>% 
      select(id, cc, time, slope, qty, qty.norm) %>%
      filter(cc==c) %>%
      arrange(slope)
    
    # Normalized actual
    actual <- ghx %>% group_by(cc) %>%
      mutate(qty.norm = qty/mean(qty)) %>%
      filter(cc==c) %>%
      arrange(slope)
    
    loss_total = loss_total+Loss(est$qty.norm, actual$qty.norm)
  }
  return(loss_total)
}


##########################
# START:::Greedy optimal window
##########################
# Greedy search
# df is by customerId, month, volume info in ONE channel-category
# Search for the "optimal" end point of the interval with step = 1
# Set step = -1 to search the other direction.
GreedySearch <- function(df, ghx, step = 1, startMethod = 'target'){
  # Get all the customer avg growth/slope info and sort by growth rate
  actual <- ghx$qty.norm
  start.slope <- ghx$slope
  
  customers <- df %>% group_by(id) %>% summarise(slope = mean(slope))
  customers.sorted <- customers[order(customers$slope),]
  customers.n <- nrow(customers)
  # define the step if needed
  step.n <- max(round(customers.n*step/100),2)
  # Function to determine the start point
  start <- SearchStart(customers.sorted, method = startMethod, target = start.slope)
  # Initial end point
  end<-start
  
  options<-list(start = c(start, start, start), 
                end = c(end, end, end))
  # customers in the [start, end] interveral
  customers.list<- list(customers.sorted[options$start[1]:options$end[1],]$id,
                        customers.sorted[options$start[2]:options$end[2],]$id,
                        customers.sorted[options$start[3]:options$end[3],]$id)
  stop.flag <- F
  # best std error
  best.se <- 100000
  # best solution
  best.end <- end
  best.start <- start
  best.idx <- 3
  while(!stop.flag){
    # Pull the customer sales history based on the customers.list and aggregate
    customers.subset1<-df[df$id %in% customers.list[[1]], ] %>% 
      group_by(time) %>% 
      summarise(qty.sum = sum(qty), norm.avg = mean(qty.norm))%>%
      mutate(qty.norm = qty.sum/mean(qty.sum))
    
    customers.subset2<-df[df$id %in% customers.list[[2]], ] %>% 
      group_by(time) %>% 
      summarise(qty.sum = sum(qty), norm.avg = mean(qty.norm))%>%
      mutate(qty.norm = qty.sum/mean(qty.sum))
    
    customers.subset3<-df[df$id %in% customers.list[[3]], ] %>% 
      group_by(time) %>% 
      summarise(qty.sum = sum(qty), norm.avg = mean(qty.norm)) %>%
      mutate(qty.norm = qty.sum/mean(qty.sum))
    
    # Calculate the std error
    est1 <- customers.subset1$qty.norm
    est2 <- customers.subset2$qty.norm
    est3 <- customers.subset3$qty.norm
    se.list <-c(Loss(est1, actual),
                Loss(est2, actual),
                Loss(est3, actual))
    # se <- Loss(est, actual)
    best.idx <- which.min(se.list)
    se <- se.list[best.idx]
    print(se)
    if(se < best.se && !is.na(sum(se.list))){
      best.se <- se
      best.end <- options$end[best.idx]
      best.start <- options$start[best.idx]
      options <- list(start = c(best.start, best.start-step.n, best.start-step.n), 
                      end = c(best.end+step.n, best.end, best.end+step.n))
      if(min(options$start)<=0 || max(options$end>=customers.n)){
        break()
      }
      customers.list<- list(customers.sorted[options$start[1]:options$end[1],]$id,
                            customers.sorted[options$start[2]:options$end[2],]$id,
                            customers.sorted[options$start[3]:options$end[3],]$id)
      stop.flag <- F
    }else{
      stop.flag <- T
    }
      
    # check the stop condition
      # all customers are included
    if( end>= customers.n || start <=1){
      stop.flag <- T
    }
    
  }
  return(list(start = best.start,
              end = best.end, 
              start.pct= round(best.start/customers.n, 2),
              end.pct = round(best.end/customers.n,2), 
              mse = best.se,
              soldto = customers.sorted$id[best.start:best.end]))
}


df<-raw %>% select(CAH.Sub.Category, Channel, SoldToCustomerId, Year.Quarter, Sales, adj.slope, GHX.Sales, GHX.adj.slope)
names(df)<- c('category', 'channel', 'id', 'quarter', 'qty', 'slope', 'ghx.qty', 'ghx.slope')
df$cc <- paste(df$category, '-', df$channel)
df<-df %>% group_by(cc, id) %>% mutate(time = row_number())
ghx <- group_by(df, time, cc) %>% 
  summarise(qty = mean(ghx.qty), slope = mean(slope)) %>%  
  arrange(desc(cc))

df$category <- NULL
df$channel <- NULL

cc.list<-unique(df$cc)

window.df <- data.frame(cc=NULL, 
                        lb=NULL, 
                        ub=NULL, 
                        lb.pct=NULL, 
                        ub.pct=NULL, 
                        groupSize=NULL,
                        mse = NULL)
soldto.df <- data.frame(cc = NULL, 
                        id = NULL)
for(c in cc.list) {
  print(c)
  df.norm <- df %>% group_by(id, cc) %>%
    mutate(qty.norm = qty/mean(qty)) %>% 
    select(id, cc, time, slope, qty, qty.norm) %>%
    filter(cc==c)
  
  ghx.norm <- ghx %>% group_by(cc) %>%
    mutate(qty.norm = qty/mean(qty)) %>%
    filter(cc==c)
  
  out <-GreedySearch(df.norm, ghx.norm, step = 2)

  c.df<-data.frame(cc=c, lb=out$start,  ub=out$end, lb.pct=out$start.pct, ub.pct=out$end.pct, groupSize = length(unique(df.norm$id)), mse = out$mse)
  window.df<-rbind(window.df, c.df)
  
  soldto.df<- rbind(soldto.df, data.frame(cc = c, id = out$soldto))
}

library(stringr)
x<-str_split_fixed(as.character(window.df$cc), " - ", 2)
window.df$category <- x[,1]
window.df$channel <- x[,2]
x<-str_split_fixed(as.character(soldto.df$cc), " - ", 2)
soldto.df$category <- x[,1]
soldto.df$channel <- x[,2]

# Disable output temprorarily
#write.csv(soldto.df, 'A:\\Teams\\SWAT\\Market Intelligence (GHX)\\data\\customizedWindow_quarter sales data-TAM version.csv')
median(window.df$lb.pct)
median(window.df$ub.pct)
head(window.df)
##########################
# END:::Greedy optimal window
##########################


##########################
# START:::Search the global window
##########################

global.window.df <- data.frame(lb=NULL, 
                                ub=NULL, 
                                mse = NULL)

global.window.surface.df <- data.frame(lb=NULL, 
                               ub=NULL, 
                               mse = NULL)

stop.flag<- F

# Initial global window
step = 0.01 # 1%
start <- 0.5 # start at 50%
end <-0.5 # end at 50%
best.start <-start
best.end <- end
best.se <- 10000000


# Aggregated df by sold-to, cc. 
# - calculate the normalized qty
# - sort by overall slope
df.agg <-df %>% group_by(id, cc) %>%
  mutate(qty.norm = qty/mean(qty)) %>% 
  select(id, cc, time, slope, qty, qty.norm)%>%
  arrange(slope)

ghx.agg <- ghx %>% group_by(cc) %>%
  mutate(qty.norm = qty/mean(qty))%>%
  arrange(slope)

while(!stop.flag){
  # Initial the total loss list
  loss_total.list <- c(0, 0, 0)
  # Calculate the mse by each cc 
  for (c in cc.list){
    est <- df.agg[which(df.agg$cc==c),]
    actual <- ghx.agg[which(ghx.agg$cc==c),]
    
    customers <- est %>% group_by(id) %>% summarise(slope = mean(slope))
    customers.sorted <- customers[order(customers$slope),]
    customers.n <- nrow(customers)
    # define the step if needed
    step.n <- max(round(customers.n*step),2)
    start.idx <- max(round(customers.n * start), 1)
    end.idx <- min(round(customers.n * end), customers.n)
    
    options <- list(start.idx = c(start.idx, start.idx-step.n, start.idx-step.n), 
                    end.idx = c(end.idx+step.n, end.idx, end.idx+step.n),
                    start = c(start, start-step, start-step), 
                    end = c(end+step, end, end+step))
    
    customers.list<- list(customers.sorted[options$start.idx[1]:options$end.idx[1],]$id,
                          customers.sorted[options$start.idx[2]:options$end.idx[2],]$id,
                          customers.sorted[options$start.idx[3]:options$end.idx[3],]$id)
    # 1) Pick selective customers within window; 
    # 2) sum the qty, then normalize
    customers.subset1<-est[est$id %in% customers.list[[1]], ] %>% 
      group_by(time) %>% 
      summarise(qty.sum = sum(qty), norm.avg = mean(qty.norm))%>%
      mutate(qty.norm = qty.sum/mean(qty.sum))
    
    customers.subset2<-est[est$id %in% customers.list[[2]], ] %>% 
      group_by(time) %>% 
      summarise(qty.sum = sum(qty), norm.avg = mean(qty.norm))%>%
      mutate(qty.norm = qty.sum/mean(qty.sum))
    
    customers.subset3<-est[est$id %in% customers.list[[3]], ] %>% 
      group_by(time) %>% 
      summarise(qty.sum = sum(qty), norm.avg = mean(qty.norm)) %>%
      mutate(qty.norm = qty.sum/mean(qty.sum))
    
    
    # Calculate the std error
    est1 <- customers.subset1$qty.norm
    est2 <- customers.subset2$qty.norm
    est3 <- customers.subset3$qty.norm
    se.list <-c(Loss(est1, actual$qty.norm),
                Loss(est2, actual$qty.norm),
                Loss(est3, actual$qty.norm))
    loss_total.list<-loss_total.list + se.list
  }
  best.idx <- which.min(loss_total.list)
  se <- loss_total.list[best.idx]
  if(se < best.se && !is.na(sum(se.list))){
    print(paste('Current se = ',round(se,4), ' Best se = ', round(best.se,4), ' | LB = ', best.start, ', UB = ', best.end))
    best.se <- se
    best.end <- options$end[best.idx]
    best.start <- options$start[best.idx]
    
    start<-best.start
    end<-best.end
    
    stop.flag <- F
  }else{
    #stop.flag <- T
    stop.flag <- F
    
    #### TBD ####
    if(end >=0.99 && start <=0.01){
      stop.flag <- T
    }else{
      end <- min(options$end[best.idx], 1)
      start <- max(options$start[best.idx], 0.01)
    }
    ##### TBD ####
  }
  
  c.df<-data.frame(lb=start,  ub=end, mse = se)
  global.window.df<-rbind(global.window.df, c.df)
}

p <- plot_ly(global.window.df, x = ~lb, y = ~ub, z = ~mse, 
             color = ~mse, colors = c('#0C4B8E', '#BF382A')) %>%
  #add_surface()%>%
  layout(scene = list(zaxis = list(title='Error'),
                      type = 'log'))

p
##########################
# END:::Search the global window
##########################




##########################
# Brutal force search the whole space
##########################

for(start in seq(0.01, 0.99, 0.01)){
  for(end in seq(start, 0.99, 0.01)){
  # Initial the total loss list
  se<-0
  # Calculate the mse by each cc 
  for (c in cc.list){
    est <- df.agg[which(df.agg$cc==c),]
    actual <- ghx.agg[which(ghx.agg$cc==c),]
    
    customers <- est %>% group_by(id) %>% summarise(slope = mean(slope))
    customers.sorted <- customers[order(customers$slope),]
    customers.n <- nrow(customers)
    # define the step if needed
    step.n <- max(round(customers.n*step),2)
    start.idx <- max(round(customers.n * start), 1)
    end.idx <- min(round(customers.n * end), customers.n)
    
    customers.list<- customers.sorted[start.idx:end.idx,]$id
                         
    
    customers.subset<-est[est$id %in% customers.list, ] %>% 
      group_by(time) %>% 
      summarise(qty.sum = sum(qty), norm.avg = mean(qty.norm))%>%
      mutate(qty.norm = qty.sum/mean(qty.sum))
    # Calculate the std error
    se.list <-Loss(customers.subset$qty.norm, actual$qty.norm)
               
    se<-se+ se.list
  }
  
  if(se < best.se){
    print(paste('Current se = ',round(se,4), ' Best se = ', round(best.se,4), ' | LB = ', best.start, ', UB = ', best.end))
    best.se <- se
    best.end <- end
    best.start <- start
  }
  else{
    print(paste('Current se = ',round(se,4)))
  }
  
  c.df<-data.frame(lb=start,  ub=end, mse = se)
  global.window.surface.df<-rbind(global.window.surface.df, c.df)
  }
}

library(plot3D)
print(' Best se = ', round(best.se,4), ' | LB = ', best.start, ', UB = ', best.end)
library(reshape)
m<-matrix(nrow = 99, ncol = 99)

for(i in 1:99){
  m[i,i:99] <- as.matrix(global.window.surface.df[round(global.window.surface.df$lb*100)==i, ]$mse)
}
p <- plot_ly( y= c(20:45), x= c(55:90) ,z = ~m[20:45, 55:90], colors = c('#0C4B8E', '#BF382A')) %>%
  add_surface() %>%
  layout(
    title = '',
    yaxis = list(
      #range = c(-0.5, 3.5), 
      title = "Lower bound"
    ),
    xaxis = list(
      #range = c(-0.5, 3.5), 
      title = "Upper bound"
    ))
p

p <- plot_ly(global.window.surface.df, x = ~lb, y = ~ub, z = m, 
             type = "surface",
             color = mse, colors = c('#0C4B8E', '#BF382A')) %>%
  #add_surface()%>%
  layout(scene = list(zaxis = list(title='Error'),
                      type = 'log'))
  
p
##########################
# Simple analysis
##########################
library(plotly)
library(ggplot2)

# Over all market growth vs GHX growth
ggplot() +
  geom_density(data = df, aes(slope, fill = cc), alpha = 0.2, colour = "grey30") +
  geom_density(data = ghx, aes(slope), size = 1)+
  xlab("Sales Growth")+
  ylab("")+
  xlim(-0.08, 0.08)+
  theme(legend.position="none")+
  geom_hline(yintercept=0, colour="white", size=1)

# Pick a specific cc
i=45
c<-cc.list[i]
df.norm <- df %>% group_by(id, cc) %>%
  mutate(qty.norm = qty/mean(qty)) %>% 
  select(id, cc, time, slope, qty, qty.norm) %>%
  filter(cc==c)

ghx.norm <- ghx %>% group_by(cc) %>%
  mutate(qty.norm = qty/mean(qty)) %>%
  filter(cc==c)

ggplot() +
  geom_density(data = df.norm, aes(slope, fill = cc), alpha = 0.2, colour = "grey30") +
  geom_vline(xintercept = mean(ghx.norm$slope), size = 1, linetype=6)+
  xlab("Sales Growth")+
  ylab("")+
  xlim(-1, 1)+
  theme(legend.position="none")+
  geom_hline(yintercept=0, colour="white", size=1)

customers <- df.norm %>% group_by(id) %>% summarise(slope = mean(slope))
customers.sorted <- customers[order(customers$slope),]
customers.list<- customers.sorted[window.df$lb[i]:window.df$ub[i],]$id
customers.subset<-df.norm[df.norm$id %in% customers.list, ] %>% 
  group_by(time)
ggplot()+
  geom_line(data = df.norm, aes(x = time, y=qty.norm, group = id), alpha = 0.1, colour = "grey30") +
  geom_line(data = ghx.norm, aes(x = time, y=qty.norm), size = 1)+
  geom_line(data = customers.subset, aes(x = time, y=qty.norm, group = id), size = 0.5, colour="red")+
  geom_point(data = ghx.norm,aes(x = time, y=qty.norm), size = 2)+
  ylim(0,2)

