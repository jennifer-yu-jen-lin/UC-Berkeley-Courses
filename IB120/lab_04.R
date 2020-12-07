## 1
#### Plot the results from the code that implements the model described above using the ggplot package instead of R’s built-in plot function. Use this as an opportunity to acquaint yourself with the various parameters in this package.
library(deSolve)
library(ggplot2)
basicSIR <- function(time, state, parameters){
  with(as.list(c(state,parameters)),
       {
         dS <- -beta*S*I
         dE <- beta*S*I - alpha*E
         dI <- alpha*E - gamma*I
         dR <- gamma*I
         return(list(c(dS,dE,dI,dR)))
       })
}
time1 <- seq(from=0, to=400, by=1)
init_cond1 <- c(S=7000000-1, E=0, I=1, R=0)
parameters1 <- c(beta=0.0000001238, gamma=1/3, alpha=1/6)
# Ode
output1 <- ode(y=init_cond1, times=time1, func=basicSIR, parms=parameters1)
output1 <- as.data.frame(output1)
# Plot
ans1 <- ggplot(output1, aes(output1$time)) + 
  geom_line(aes(y = output1$S, colour = "S"),lwd=1) + 
  geom_line(aes(y = output1$E, colour = "E"),lwd=1) + 
  geom_line(aes(y = output1$I, colour = "I"),lwd=1) + 
  geom_line(aes(y = output1$R, colour = "R"),lwd=1) +
  xlab("time (days)") +
  ylab("population")
print(ans1)

## 2
#### Calculate the number of infected patients after 30 days
daysAfter <- output1$time[31]
infectedPatients <- output1$I[31]
ans2 <- paste0("The number of infected patients after ", daysAfter ," days is ", infectedPatients)
print(ans2)

## 3
#### How many people must be vaccinated to stop the infection?
# Vaccinated rate = delta
# Vaccinated people = V
vaccinatedSIR <- function(time, state, parameters){
  with(as.list(c(state,parameters)),
       {
         dS <- -beta*S*I - delta*S
         dE <- beta*S*I - alpha*E
         dI <- alpha*E - gamma*I
         dR <- gamma*I
         dV <- delta*S
         return(list(c(dS,dE,dI,dR,dV)))
       })
}
## try 1/1000
time3_1 <- seq(from=0, to=400, by=1)
init_cond3_1 <- c(S=7000000-1-7000, E=0, I=1, R=0, V=7000)
parameters3_1 <- c(beta=0.0000001238, gamma=1/3, alpha=1/6, delta=1/1000)
# Ode
output3_1 <- ode(y=init_cond3_1, times=time3_1, func=vaccinatedSIR, parms=parameters3_1)
output3_1 <- as.data.frame(output3_1)
# Plot
ans3_1 <- ggplot(output3_1, aes(output3_1$time)) + 
  geom_line(aes(y = output3_1$S, colour = "S"),lwd=1) + 
  geom_line(aes(y = output3_1$E, colour = "E"),lwd=1) + 
  geom_line(aes(y = output3_1$I, colour = "I"),lwd=1) + 
  geom_line(aes(y = output3_1$R, colour = "R"),lwd=1) +
  geom_line(aes(y = output3_1$V, colour = "V"),lwd=1) +
  xlab("time (days)") +
  ylab("population")
print(ans3_1)
print(paste0("Maximum I: ", max(output3_1$I)))

## try 1/100
time3_2 <- seq(from=0, to=400, by=1)
init_cond3_2 <- c(S=7000000-1-70000, E=0, I=1, R=0, V=70000)
parameters3_2 <- c(beta=0.0000001238, gamma=1/3, alpha=1/6, delta=1/100)
# Ode
output3_2 <- ode(y=init_cond3_2, times=time3_2, func=vaccinatedSIR, parms=parameters3_2)
output3_2 <- as.data.frame(output3_2)
# Plot
ans3_2 <- ggplot(output3_2, aes(output3_2$time)) + 
  geom_line(aes(y = output3_2$S, colour = "S"),lwd=1) + 
  geom_line(aes(y = output3_2$E, colour = "E"),lwd=1) + 
  geom_line(aes(y = output3_2$I, colour = "I"),lwd=1) + 
  geom_line(aes(y = output3_2$R, colour = "R"),lwd=1) +
  geom_line(aes(y = output3_2$V, colour = "V"),lwd=1) +
  xlab("time (days)") +
  ylab("population")
print(ans3_2)
print(paste0("Maximum I: ", max(output3_2$I)))

## try 1/100 - 10/100
max_I <- c()
for(i in 1:10){
  V_i <- 7000000*i/100
  time3_3 <- seq(from=0, to=400, by=1)
  init_cond3_3 <- c(S=7000000-1-V_i, E=0, I=1, R=0, V=V_i)
  parameters3_3 <- c(beta=0.0000001238, gamma=1/3, alpha=1/6, delta=i/100)
  output3_3 <- as.data.frame(ode(y=init_cond3_3, times=time3_3, func=vaccinatedSIR, parms=parameters3_3))
  max_I[i] <- max(output3_3$I)
}

max_I[6]
max_I[7]




## 4
#### Think about how to modify this model to be more realistic (e.g. account for births and deaths)

## 5
#### Alternate Assignment: Create your own model, implement it, plot the results
#### and describe briefly the justification and assumptions of such model.
# Birth rate = br
# Birth = B
# Death rate (natural) = drN
# Death (natural) = DN
# Death rate (disease) = drD
# Death (disease) = DD
mySIR <- function(time, state, parameters){
  with(as.list(c(state,parameters)),
       {
         dS <- -beta*S*I - delta*S + br*(S+E+I+R+V)*(1-delta) - drN*S
         dE <- beta*S*I - alpha*E - drN*E
         dI <- alpha*E - gamma*I - drN*I - drD*I
         dR <- gamma*I - drN*R
         dV <- delta*S + br*(S+E+I+R+V)*delta - drN*V
         dB <- br*(S+E+I+R+V)
         dDN <- drN*S + drN*E + drN*I + drN*R + drN*V
         dDD <- drD*I
         return(list(c(dS,dE,dI,dR,dV,dB,dDN,dDD)))
       })
}
time5 <- seq(from=0, to=400, by=1)
init_cond5 <- c(S=7000000-1-7000, E=0, I=1, R=0, V=7000, B=0, DN=0, DD=0)
parameters5 <- c(beta=0.0000001238, gamma=1/3, alpha=1/6, delta=1/1000, br=0.00001, drN=0.00001, drD=0.001)
# Ode
output5 <- ode(y=init_cond5, times=time5, func=mySIR, parms=parameters5)
output5 <- as.data.frame(output5)
# Plot
ans5 <- ggplot(output5, aes(output5$time)) + 
  geom_line(aes(y = output5$S, colour = "S"),lwd=1,linetype = "dashed") + 
  geom_line(aes(y = output5$E, colour = "E"),lwd=1,linetype = "dashed") + 
  geom_line(aes(y = output5$I, colour = "I"),lwd=1,linetype = "dashed") + 
  geom_line(aes(y = output5$R, colour = "R"),lwd=1,linetype = "dashed") +
  geom_line(aes(y = output5$V, colour = "V"),lwd=1,linetype = "dashed") +
  geom_line(aes(y = output5$V, colour = "B"),lwd=1,linetype = "dashed") +
  geom_line(aes(y = output5$V, colour = "DN"),lwd=1,linetype = "dashed") +
  geom_line(aes(y = output5$V, colour = "DD"),lwd=1,linetype = "dashed") +
  xlab("time (days)") +
  ylab("population")
print(ans5)













################################################################################
plot(output$time, output$S, type="l", col="blue", lwd=2, xlab="time", ylab="individuals")
lines(output$time, output$E, col="darkorange", lwd=2)
lines(output$time, output$I, col="red", lwd=2)
lines(output$time, output$R, col="darkgreen", lwd=2)
legend("right",legend=c("S","E","I","R"), col=c("blue","darkorange","red","darkgreen"))

