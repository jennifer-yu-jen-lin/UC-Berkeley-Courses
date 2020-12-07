# 1. Come up with a system of equations with 3 unknowns and 3 equations 
#     (in contrast to the example with 2 unknowns and 2 equations) and solve it.

A <- rbind(c(1,5,7), c(8,3,8), c(5,7,2))
b <- c(9,3,1)
x <- solve(A,b)
print(x)


# 2. Solve the following differential equation exactly as was done in the example. 
#     You may do it by hand or with code:

# install.packages('deSolve',repos='http://probability.ca/cran')
library(deSolve)

time2 <- seq(from=0, to=50, by=0.01)
parameters2 <- c(alpha) # alpha ?????
state2 <- c(x=) # x ?????

func2 <- function(time2, state2, parameters2){
  dx <- parameters['alpha']*state['x']*state['x']
  return(list(dx))
}

output2 <- ode(y=state2, times=time2, func=func2, parms=parameters2)


# 3. Write code that models the predator-prey relationship (Lotka-Volterra model).
time3 <- seq(from=0, to=50, by=0.01)
parameters3 <- c(r=1.8, k=0.4, e=0.12, d=1)
state3 <- c(V=1, P=3)  # just try some numbers

func3 <- function(time3, state3, parameters3){
  # c in the equation is actually k
  dV = parameters3['r']*state3['V']-parameters3['k']*state3['V']*state3['P']
  dP = parameters3['e']*parameters3['k']*state3['V']*state3['P'] - parameters3['d']*state3['P']
  # return(c(list(dV),list(dP)))
  return(list(c(dV,dP)))
}

output3 <- ode(y=state3, times=time3, func=func3, parms=parameters3)
plot(output3)
library(ggplot2)
q <- ggplot(data=as.data.frame(output),aes(time,x)) + geom_point()
print(q)

########################################################
# practice in class

# 1
A <- rbind(c(5,3.5), c(1,1))
b <- c(4.38, 1)
x <- solve(A,b)

# 3

time <- seq(from=0, to=50, by=0.01)
parameters <- c(r=0.4, K=13)
# e.g. parameters['r']
state <- c(x=0.1)

logistic <- function(time, state, parameters){
  dx <- parameters['r']*state['x']*(1-state['x']/parameters['K'])
  return(list(dx))
}

logistic <- function(time, state, parameters){
  with(as.list(c(state,parameters)){
    dx <- r*x*(1-x/K)
    return(list(dx))
  }
  )
}

output <- ode(y=state, times=time, func=logistic, parms=parameters)

plot(output)


library(ggplot2)
q <- ggplot(data=as.data.frame(output),aes(time,x)) + geom_point()
print(q)



