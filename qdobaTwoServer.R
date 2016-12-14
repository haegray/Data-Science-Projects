

# Qdoba line Discrete Event Simulation
# CPSC 420 -- spring 2016

library(doParallel)
registerDoParallel(8)

#two workers

IDLE <- 0
BUSY <- 1

PRINT.STUFF <- FALSE

cat0 <- function(...) {
  if (PRINT.STUFF) {
    cat(...,sep="")
  }
}


num.mins.open <- 1000

arrival.rate <- 1    # cust/min
service.rate.one <- .2 #cust/min
service.rate.two<- .3 # cust/min




run.qdoba.sim <- function(arrival.rate=1, service.rate.one=.2, service.rate.two=.3, 
                          num.mins.open=1000) {
  
  next.arrival <- rexp(1, arrival.rate)
  #customer is getting beans, cheese, rice
  next.service.one <- Inf
  #customer is getting guac, salsa, and paying
  next.service.two<- Inf
  
  
  total.customers.served <- 0
  max.line.size <- 0
  time.spent.busy <- 0 
  total.cumulative.line.size <- 0
  
  arrival.times <- vector()
  service.times <- vector()
  
  #employee greeting customers and serving beans, cheese, rice
  server.state.one <- IDLE
  #employee serving guac, salsa, and taking payment
  server.state.two <- IDLE
  num.in.line <- 0
  
  sim.clock <- 0
  
  while (sim.clock <= num.mins.open  ||  server.state.one == BUSY || server.state.two == BUSY) {
    
    # Compute the time of the next event.
    time.of.next.event.one <- min(next.arrival,next.service.one)
    #determines if someone is arriving or presently getting beans, cheese, and rice
    time.of.next.event.two<-min(next.service.one,next.service.two)
    #determines if someone is finished getting beans and cheese or if they're paying and leaving
    time.of.next.event<-min(next.arrival,next.service.one,next.service.two)
    
    # How much time has elapsed between the previous event and this 
    # one?
    time.since.last.event.one<-time.of.next.event.one - sim.clock
    time.since.last.event.two<-time.of.next.event.two - sim.clock
    time.since.last.event <- time.of.next.event - sim.clock
    
    # Compute the height of the rectangles we need to add to our
    # running totals. (This is the state that the server was just in,
    # and the number of people in line there just were.)
    height.of.util.rectangle.one <- server.state.one
    height.of.util.rectangle.two <-server.state.two
    height.of.num.in.line.rectangle <- num.in.line
    
    # Compute the areas of the rectangles we need to add to our
    # running totals.
    area.of.util.rectangle.one <- time.since.last.event.one *
      height.of.util.rectangle.one
    area.of.util.rectangle.two<- time.since.last.event.two * height.of.util.rectangle.two
    
    area.of.num.in.line.rectangle <- time.since.last.event.one *
      height.of.num.in.line.rectangle
    
    # Add these areas to our running totals.
    time.spent.busy <- time.spent.busy + area.of.util.rectangle.one + area.of.util.rectangle.two
    total.cumulative.line.size <- total.cumulative.line.size +
      area.of.num.in.line.rectangle
    
    
    # Okay, now that that bookkeeping's out of the way, actually
    # advance the simulation clock.
    sim.clock <- time.of.next.event
    
    if (next.arrival < next.service.one) {
      cat0("A customer arrived at ", sim.clock, ".\n")
      arrival.times <- c(arrival.times, next.arrival)
      # The next event to happen is an arrival.
      if (server.state.one == BUSY) {
        num.in.line <- num.in.line + 1   
        if (num.in.line > max.line.size) {
          max.line.size <- num.in.line
        }
        cat0("  Had to get in line. :( (line is now ", 
             num.in.line, " long.)\n")
      } else {
        cat0("  Step right up!\n")
        server.state.one <- BUSY 
        #HOW DO I SWITCH TO SERVER TWO ONCE BURRITO IS MADE
        next.service.one <- sim.clock + rexp(1, service.rate.one)
      }
      
      if (sim.clock <= num.mins.open) {
        next.arrival <- sim.clock + rexp(1, arrival.rate)
      } else {
        cat0("Sorry, bub, we're closed!!\n")
        next.arrival <- Inf
      }
      }else if (next.service.one < next.service.two) {
        if (server.state.two == BUSY) {
          #delay
          #num.in.line <- num.in.line + 1   
          #if (num.in.line > max.line.size) {
          #max.line.size <- num.in.line
          #}
          #cat0("Waiting to get guac and check out ", 
               #num.in.line, " long.)\n")
        } else {
          cat0("Time to pay up!\n")
          server.state.two <- BUSY 
          next.service.two <- sim.clock + rexp(1, service.rate.two)
        }
      cat0("A customer got served at ", sim.clock, ".\n")
      service.times <- c(service.times, next.service.two)
      total.customers.served <- total.customers.served + 1
      # The next event to happen is a service.
    }    
      if (num.in.line > 0) {
        cat0("  The line advances! It now has ", num.in.line, 
             " people in it.\n")
        num.in.line <- num.in.line - 1
        next.service <- sim.clock + rexp(1, service.rate.one)
      } else {
        cat0("  Whew! Get to sit down finally!\n")
        server.state.one <- IDLE
        server.state.two <- IDLE
        next.service <- Inf
      }
    }

  
  
  
  
  
  cat0("It's now quittin' time!\n")
  cat0("We served ", total.customers.served, " customers today!\n")
  cat0("The longest the line ever got was: ",max.line.size, ".\n")
  cat0("The server was on his feet ", round(time.spent.busy / sim.clock, 2) * 100, "% of the time.\n")
  cat0("The average line size was ", round(total.cumulative.line.size / sim.clock, 2), " people.\n")
  
  delays <- service.times - arrival.times
  
  cat0("The average delay in line was ",mean(delays)," mins.\n")
  
  cat0("The store closed today after ", sim.clock, " minutes.\n")
  
  return(data.frame(arrival.rate=arrival.rate,
                    service.rate=service.rate.one + service.rate.two,
                    total.customers.served=total.customers.served,
                    max.line.size=max.line.size,
                    utilization=time.spent.busy / sim.clock * 100,
                    average.line.size=total.cumulative.line.size / sim.clock,
                    average.delay=mean(delays),
                    time.store.close=sim.clock))
}


results <- foreach (service.rate=seq(.1,2,.05), .combine=rbind) %dopar% {
  return(run.qdoba.sim(arrival.rate=1, service.rate.one=service.rate.one,service.rate.two=service.rate.two))
}

for (col in 3:ncol(results)) {
  plot(results$service.rate, results[[col]], type="l", col="blue",
       main=names(results)[col])
  readline()
}