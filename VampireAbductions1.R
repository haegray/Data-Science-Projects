# Aliens v. Vampires -- version 2.1
# (logistic growth with human reproduction)
# CPSC 420 -- spring 2016


# Set up time.
delta.t <- 1   # years
time <- seq(1940,2500,delta.t)

# Utility functions to convert between i and t.
itot <- function(i) (i-1)*delta.t + 1940
ttoi <- function(t) (t-1940)/delta.t + 1

# Simulation parameters.
init.human.pop <- 7e9
alien.abduction.rate <- 30000      # (beings/year)/year
bite.rate <- .1                # (people/year)/vampire
birth.rate <- .01              # (people/year)/person
sacrifice.rate<- .1 #people/year/vampire
#There is no specific rate for vampires being abducted per year because it is a portion of the alien abduction rate
A <- vector()
V <- vector()
H <- vector()
earth.population <- vector()
V.abductions<-vector()
sacrifice<- vector()

# Initial conditions. (No aliens until 1940, and only one lonely vampire.)
A[1] <- 0
V[1] <- 1
H[1] <- init.human.pop
earth.population[1] <- V[1] + H[1]
V.abductions[1]<-0
sacrifice[1]<-0


# Simulate.
for (i in 2:length(time)) {
  
  logistic.factor <- H[i-1]/earth.population[i-1]
  
  # Compute flows.
  VA.prime<-alien.abduction.rate * (time[i]-1940) * (V[i-1]/earth.population[i-1]) #vampires abducted/year
  sacrifice.prime<- sacrifice.rate * (V[i-1]) #sacrifice/year
  V.prime <- bite.rate * V[i-1] * logistic.factor - VA.prime# people/year
  A.prime <- alien.abduction.rate * (time[i] - 1940) * (logistic.factor) + VA.prime   # beings/year
  H.prime <- H[i-1] * birth.rate - (V.prime + (A.prime - VA.prime) + sacrifice.prime)    # people/year
  earth.population.prime <- -A.prime - sacrifice.prime    # people/year
  
  if(H[i-1] - sacrifice[i-1]<0){
    V.prime <- V.prime - (abs(sacrifice[i-1] - H[i-1])*sacrifice.rate)
    
  } else {
    V.prime <- bite.rate * V[i-1] * logistic.factor - VA.prime# people/year
  }
  
  
  # Compute stocks.
  if (A[i-1] + A.prime * delta.t <= 0){
    A[i]<-0
  } else {
    A[i] <- A[i-1] + A.prime * delta.t 
  }
  
  # people abducted
  
  if( V[i-1] + V.prime * delta.t <= 0 ) {
    V[i] <-0
  } else {
    V[i] <- V[i-1] + V.prime * delta.t 
  }               # vampires
  
  if(H[i-1] + H.prime * delta.t <= 0 ){
    H[i]<-0
  } else {
    H[i]<-H[i-1] + H.prime * delta.t
  } #humans left
  
  if(V[i] + H[i] <= 0 ){
    earth.population[i]<-0
    }else{
      earth.population[i] <- V[i] + H[i]
    } 
  
  # people
  
  V.abductions[i]<- V.abductions[i-1] + VA.prime * delta.t #total vampires abducted thus far
  sacrifice[i]<-sacrifice[i-1] + sacrifice.prime * delta.t #total victims sacrificed to vampires thus far 
}


# Plot results.
all.values <- c(A,V,H,earth.population)
plot(time,A,type="l",col="green",lwd=2,
     ylim=c(min(all.values),max(all.values)),
     main="Aliens v. Vampires apocalypse -- oh my!!",
     xlab="year",
     ylab="# of victims")
lines(time,V,col="red",lwd=2)
lines(time,H,col="black",lwd=1)
lines(time,earth.population,col="brown",lty="dotted",lwd=3)
lines(time,V.abductions,col="purple",lty="dashed",lwd=1)
lines(time,sacrifice,col="blue", lty = 3, lwd=1)
legend("topleft",legend=c("Alien abductions","Vampire bites","Humans","Earthlings", "Vampires Abducted","Human Victims"),
       fill=c("green","red","black","brown", "purple","blue"))