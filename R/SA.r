# Demo of Simulate annealing
# Author: Yan.W


# Define Loss function (Minimie the objective function)
Loss <- function(x){
  #return(1/(x*sin(x)+12 ))
  return(x^2)
}

# Initialization
UPPER = 1
LOWER = -1
range_of_search <-(UPPER-LOWER)/20
stmp <- runif(100,LOWER,UPPER)
t0 <- var(C(stmp))            # initial tempreture
s0 <- runif(1,LOWER,UPPER)    # Set initial status
ITERATIONS <- 3000                 # Iterations
ccnt <- 200                   # Stop condition, if no better solution in ccnt iterations, then stop 
best.solution <- UPPER        # Initial best solution 
ccntVc <- NULL

for (t in 1:ITERATIONS){ 
  # Generate a feasible new solution near S0
  s1 <- rnorm(1,mean = s0, sd = range_of_search)
  while(s1<LOWER || s1>UPPER){
    # If infeasible, generate another one
    s1 <- rnorm(1,mean = s0, sd = range_of_search)
  }
  # Calculate the delta of tempreture
  delta_t <- Loss(s1) - Loss(s0)
  if(delta_t < 0){
    s0 <- s1
    ccntVc <- c(ccntVc,1)
  }
  else{
    # Have a small chance to jump out (agaisnt the local optimal solution)
    p = exp(-delta_t/t0)
    if(runif(1,0,1) < p){
      s0 <- s1
      ccntVc <- c(ccntVc,1)
    }
    else{
      ccntVc <- c(ccntVc,0)
    }
  }
  # Update the best solution. 
  best.solution <- ifelse(Loss(s1)<Loss(best.solution),s1,best.solution)
  best.solution <- ifelse(Loss(s0)<Loss(best.solution),s0,best.solution)
  # Update the tempreture: Cooling down
  t0 <- t0/log(1+t)
  # Check the stop condition
  if(NROW(ccntVc)>ccnt && sum(ccntVc[(NROW(ccntVc)-ccnt+1):NROW(ccntVc)])==0){
    print(paste("No better solution over ",ccnt," iterations. End the search"))
    break;
  }
}
print(s0)
print(t)
print(best.solution)