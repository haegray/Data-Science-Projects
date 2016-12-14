library(doParallel)
registerDoParallel(5)

force.regenerate.results <- FALSE
time.delay.animation <- .1         # seconds

# Set up our time increment and our vector (array) of x (time) values
deltaX = .01   # day
x = seq(0,300,deltaX)

# itot() and ttoi() functions (if desired).
itot <- function(i) (i-1)*delta.t + 0
ttoi <- function(t) (t-0)/delta.t + 1

# Simulation parameters (inputs).
probBit = 0.3           # 1/day (daily probability of being bitten by mosq)
recoveryRate = 0.3      # 1/day (daily probability of human recovering)
immunityRate = 0.01     # 1/day (daily probability of human becoming immune)
malDeathRate = 0.005    # 1/day (daily probability of human dying)
probBiteHuman = 0.3     # 1/day (daily probability of biting a human)
mosqBirthRate = 0.01    # 1/day (mosquitos born per mosquito per day)
mosqDeathRate = 0.01    # 1/day (daily probability of mosquito dying)
recoveryRate = .3       # 1/day
initUninfHumans = 300   # humans (number of uninfected humans)
initHumanHosts = 1      # humans (number of people infected)
initImmune = 0          # humans (number of people recovered and immune)
initUninfMosq = 300     # mosquitos (number of uninfected mosquitos)
initVectors = 0         # mosquitos (number of infected mosquitos)

# Stocks. (Create a vector and an initial condition for each.)
UninfHumans = vector(length=length(x))
UninfHumans[1] = initUninfHumans
HumanHosts = vector(length=length(x))
HumanHosts[1] = initHumanHosts
Immune = vector(length=length(x))
Immune[1] = initImmune
UninfMosq = vector(length=length(x))
UninfMosq[1] = initUninfMosq
Vectors = vector(length=length(x))
Vectors[1] = initVectors

# Simulation loop. Only generate these time-consuming results if have to or
# we're explicitly instructed to.
if (!exists("grand.results")  ||  force.regenerate.results) {
  
  # Remember what time this started, just so we can brag about our performance
  # by running in parallel.
  first <- proc.time()
  
  grand.results <- foreach (mosqBirthRate = seq(0,1,1/30001), .combine=rbind) %dopar% {
      
      one.mosqBirthRate <- data.frame()
      
        for (i in 2:length(x)) {
          totalMosquitos = Vectors[i-1] + UninfMosq[i-1]
          totalHumans = UninfHumans[i-1] + HumanHosts[i-1] + Immune[i-1]
          
          # Compute rates of change.
          UninfHumansPrime = HumanHosts[i-1] * recoveryRate -
            UninfHumans[i-1] * probBit * Vectors[i-1] / totalMosquitos
          
          HumanHostsPrime = 
            UninfHumans[i-1] * probBit * Vectors[i-1] / totalMosquitos - 
            HumanHosts[i-1] * recoveryRate -
            HumanHosts[i-1] * malDeathRate -
            HumanHosts[i-1] * immunityRate
          
          ImmunePrime = HumanHosts[i-1] * immunityRate
          
          UninfMosqPrime = totalMosquitos * mosqBirthRate -
            UninfMosq[i-1] * mosqDeathRate -
            UninfMosq[i-1] * probBiteHuman * HumanHosts[i-1] / totalHumans
          
          VectorsPrime = 
            UninfMosq[i-1] * probBiteHuman * HumanHosts[i-1] / totalHumans -
            Vectors[i-1] * mosqDeathRate
          
          # Increase or decrease the population of each category based on
          #   the rates of change for this time step. (Euler's method.)
          UninfHumans[i] = UninfHumans[i-1] + UninfHumansPrime * deltaX
          HumanHosts[i] = HumanHosts[i-1] + HumanHostsPrime * deltaX
          Immune[i] = Immune[i-1] + ImmunePrime * deltaX
          UninfMosq[i] = UninfMosq[i-1] + UninfMosqPrime * deltaX
          Vectors[i] = Vectors[i-1] + VectorsPrime * deltaX
        }
      one.mosqBirthRate <- rbind(one.mosqBirthRate, 
                                 data.frame(mosqBirthRate=mosqBirthRate,
                                            UninfHumans=UninfHumans,
                                            HumanHosts=HumanHosts,
                                            Immune=Immune, 
                                            UninfMosq=UninfMosq, 
                                            Vectors=Vectors
                                 ))
      return(one.mosqBirthRate)
    }
       
  # Print the elapsed time (current time minus what it was when we started).
  print(proc.time() - first)

}
# (Relabel data frame rows because it's just too annoying otherwise.)
rownames(grand.results) <- 1:nrow(grand.results)


# Plot as an animation. Each frame of the animation corresponds to a
# particular value of mouse.birth.rate, and plots nutrition.factor vs. our two
# population minima (mice, and bats).
for (mosqBirthRate in unique(grand.results$mosqBirthRate)) {
  
  # Get just the results for the current mosqBirthRate.
  the.results <- grand.results[grand.results$mosqBirthRate == mosqBirthRate,]
  
  # Plot our two dependent variables (simulation outputs) versus our
  # simulation parameter (simulation input). Use a constant ylim so the axes
  # scales don't change from frame to frame, which would confuse us.
  
  plot(x,the.results$UninfHumans, type="n", lwd=2,col="blue",lty="dashed",
       xlab="time (days)",ylab="population", ylim=c(0,400),
       main=paste("Malaria - Humans & Mosquitos: Mosquito Birth Rate =",mosqBirthRate))
  
  lines(x,the.results$HumanHosts,lwd=2,col="blue",lty="solid")
  lines(x,the.results$Immune,lwd=2,col="green",lty="solid")
  lines(x,the.results$UninfMosq,lwd=2,col="black",lty="dashed")
  lines(x,the.results$Vectors,lwd=2,col="black",lty="solid")
  
  legend(x="topright",legend=c("uninfected humans","human hosts","immune humans","uninfected mosquitos","mosquito vectors"),lty=c("dashed","solid","solid","dashed","solid"),col=c("blue","blue","green","black","black"),lwd=2)
  
  # Delay between animation frames.
  Sys.sleep(time.delay.animation)
}

