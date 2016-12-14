# Aliens v. Vampires -- version 2.1
# (logistic growth with human reproduction)
# CPSC 420 -- spring 2016


# Set up time.
delta.t <- 1   # years
time <- seq(1940,2400,delta.t)

# Utility functions to convert between i and t.
itot <- function(i) (i-1)*delta.t + 1940
ttoi <- function(t) (t-1940)/delta.t + 1

# Simulation parameters.
init.human.pop <- 7e9
alien.abduction.rate <- 30000     # (beings/year)/year
bite.rate <- .3                # (people/year)/vampire
birth.rate <- .3             # (people/year)/person
sacrifice.rate<- 3 #people/year/vampire
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
  if(is.na(logistic.factor))logistic.factor<-0

  
  
  # Compute flows.
  VA.prime<-alien.abduction.rate * (time[i]-1940) * (V[i-1]/earth.population[i-1]) #vampires abducted/year
  if(is.na(VA.prime))VA.prime<-0
  
  
  if(H[i-1] < 1){
    sacrifice.prime<-0
    sacrifice[i]<-0
    H[i]<-0
    V.prime<-(-sacrifice.rate * V[i-1] - VA.prime)
    
  } else {
    sacrifice.prime<- sacrifice.rate * (V[i-1] * logistic.factor) #sacrifice/year
  }
 
  
  
  if(H[i-1]<=sacrifice[i-1] || sacrifice.rate==0){
    V.prime <- ((sacrifice[i-1] - sacrifice.rate * V[i-1])  - VA.prime)
  } else {
    V.prime <- bite.rate * V[i-1] * logistic.factor - VA.prime # people/year
  }
  if(is.na(V.prime))V.prime<-0
  
  
  
  if(H[i-1]<1 && V[i-1]<1){
    A.prime<-0
    H[i]<-0
    V[i]<-0
    sacrifice[i]<-0
    V.abductions[i]<-0
  } else {
  A.prime <- alien.abduction.rate * (time[i] - 1940) * (logistic.factor) + VA.prime 
  }# beings/year
  
  if(V.prime<0){
    H.prime <- H[i-1] * birth.rate + (V.prime + (A.prime - VA.prime) + sacrifice.prime)
  } else {
  H.prime <- H[i-1] * birth.rate - (V.prime + (A.prime - VA.prime) + sacrifice.prime) 
  }# people/year
  earth.population.prime <- -A.prime - sacrifice.prime    # people/year
  
  
  
  
  # Compute stocks.
  if (A[i-1] + A.prime * delta.t <1 || V[i-1] <1 && H[i-1] <1){
    A.prime<-0
    sacrifice[i]<-0
    A[i]<-A[i-1]
  } else {
    A[i] <- A[i-1] + A.prime * delta.t 
  }
  
  # people abducted
  
  if( V[i-1] + V.prime * delta.t < 1 || V[i-1] < 1 ) {
    V[i] <-0
    V.abductions[i]<-0
    sacrifice[i]<-0
  } else {
    V[i] <- V[i-1] + V.prime * delta.t 
    V.abductions[i]<- V.abductions[i-1] + VA.prime * delta.t #total vampires abducted thus far
  }               # vampires
  
  if(H[i-1] + H.prime * delta.t < 1 || H[i-1] <1 ){
    H[i]<-0
    sacrifice[i]<-0
  } else {
    H[i]<-H[i-1] + H.prime * delta.t
  } #humans left
  
  if(V[i] + H[i] < 1 ){
    earth.population[i]<-0
    }else{
      earth.population[i] <- V[i] + H[i]
    } 
  
  # people
  if(V[i] < 1 || H[i]<1 ){
    sacrifice[i]<-0
  } else {
  sacrifice[i]<-sacrifice.prime * delta.t #total victims sacrificed to vampires thus far 
}
}

# Plot results.
all.values <- c(A,V,H,earth.population)
plot(time[seq(1,length(time),10)],A[seq(1,length(V),10)],pch=1,
     ylim=c(min(all.values),max(all.values)),
     main="Aliens v. Vampires apocalypse -- oh my!!",
     xlab="year",
     ylab="# of victims")
points(time[seq(1,length(time),10)],V[seq(1,length(V),10)],pch=2)
points(time[seq(1,length(time),10)],H[seq(1,length(V),10)],pch=15)
points(time[seq(1,length(time),10)],earth.population[seq(1,length(V),10)],pch=8)
points(time[seq(1,length(time),10)],V.abductions[seq(1,length(V),10)],pch=10)
points(time[seq(1,length(time),10)],sacrifice[seq(1,length(V),10)],pch= 6)
legend("topleft",legend=c("Alien abductions","Vampire bites","Humans","Earthlings", "Vampires Abducted","Human Victims"), bty = "n",
       cex = .75, pch=c(1,2,15,8,10,6))