coinflip <- function() {
  x1 <- runif(1, 0, 1)
  if (x1 > 0.5) {
    yt <- 1
  } else {
    yt <- 0
  }
  return(yt)
}


runSim = function(numBoxes) {
  currentBox <- 0
  numFlips <- 0
  
  while (currentBox < numBoxes) {
      
    flip <- coinflip()
    if (flip == 1) {
      currentBox <- currentBox + 1
    } else {
      if (currentBox == 0) {}
      else {
        currentBox <- currentBox - 1
      }
    }
    numFlips <- numFlips + 1
  }
  return(numFlips)
}


findAvg <- function(numBoxes) {
  vecNumFlips = c()
  
  for (i in 1:2000) {
    vecNumFlips <- append(vecNumFlips, runSim(numBoxes))
  }
  return(mean(vecNumFlips))
}


plotNAverages <- function(N) {
  averages <- c()
  
  #if (file.exists("averages.RData")) {
  #  averages <- load("averages.RData")
  #  print(averages)
  #}
  
  for (i in length(averages) + 1:N) {
    print(i)
    averages <- append(averages, findAvg(i))
    print(averages[i])
  }
  
  # save(averages, file="averages.RData")
  
  plot(1:N, averages, xlab="number of boxes", ylab="average escape time", pch="$", col="purple", bg="orange")
}

plotNAverages(25)
