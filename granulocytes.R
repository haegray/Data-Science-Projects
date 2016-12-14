

# Set up our time increment and our vector (array) of x (time) values
deltaT <- 6   # day
t <- seq(0,240,deltaT)

#Simulation parameters

destructionRate<-.1 #destruction rate per day

v <- 8.2e+9 #steady state number of white blood cells

c <- destructionRate * 6 #destruction constant per time step of 6 days
init.x<-.75*v # initial white blood cells
init.p<-0 #initial white blood cells produced
init.d<-0 #initial white blood cells destroyed

b <- 8.7 #positive constant
r <- .37 #positive constant

#Stocks
x <- vector(length=length(t)) # number of White blood cells
x[1] <- init.x #number of white blood cells at time ti
xp <- vector(length=length(t)) # number of White blood cells produced
xp[1] <- init.p #number of white blood cells produced at time ti
d <- vector(length=length(t)) # number of White blood cells destroyed
d[1] <- init.d #number of white blood cells destroyed at time ti

#Steady state conditions
vd <- destructionRate *6 * v #rate of cell disappearance under steady state conditions cells/kg
# a point on the graph of the production function
#p(v) = .6v

vp <- vd #steady state production rate should be equal
#to cell disappearance rate units in healthy
#individuals units in cells/kg

maxp <- vp * 2 #maximum production rate
#this is another point on the graph although 
#the value at which the max is attained
#must be determined

#production function
p<-function(x){
  zp <- b*x*exp(1)^(-x/v*r)
  zp
}

#p(0)=0
      
      for (i in 2:(length(t))) {
        # Compute flows
        d.prime <- c * x[i-1] #destruction of granulocytes (cells/kg/ 6 days)
        xp.prime <- p(x[i-1]) #production of granulocytes (cells/day)
        x.prime <-(1-c)*x[i-1] + xp.prime #change in number of granulocytes (cells/kg/day)
        #Compute stocks
        x[i] <- x.prime 
        xp[i] <- xp.prime
        d[i] <- d.prime
        
        if(xp[i]==0){
          break;
          
        } #if number of granulocytes is zero break the loop
        
        #if(is.TRUE(xp[i] == (maxp)){
        #decline production rate
        #}
      } 
xp<-xp[!is.na(xp)]
xp<-xp[seq(1,i)]
x<-x[!is.na(x)]
x<-x[seq(1,i)]
d<-d[!is.na(d)]
d<-d[seq(1,i)]
t<-t[seq(1,i)]




print(d)
print(xp)
print(x)
print(length(t))
print(t)


all.values <- c(xp,d,x)
#plot

print(which.max(xp))
print(which.max(x))

plot(x,xp,type="l",col="red",lwd=2,ylim=c(min(all.values),max(all.values)), xlim=c(min(t),max(t)),xlab="Number of granulocytes",ylab="Production of granulocytes",
     main=paste("Number of granulocytes"))

  plot(t,xp,type="l",
       col="red",lwd=2,
       ylim=c(min(all.values),max(all.values)), xlim=c(min(t),max(t)), xlab="Time (days)", ylab="Cell Amount",
       main=paste("Number of WBC over time, constants at steady state, granulocytes at steady state"))
  lines(t,d,col="black",
        lwd=2)
  lines(t,x,col="blue",
        lwd=2)
  legend("bottomright",fill=c("black","red","blue"),legend=c("Destruction (cells/kg/6 days)","Production (cells/kg/6 days)","Granulocytes (cells/kg)"), bty="n")
  
  
