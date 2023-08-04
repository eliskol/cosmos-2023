manyRandomWalks <- function(n.walks, n.flips, p) {
  x.mat <- matrix(NA, n.flips, n.walks)
  for (i in 1:n.walks) {
    x.start <- 0
    x <- x.start
    x.vec <- c(x.start)
    for (j in 1:(n.flips - 1)) {
      x <- x.vec[length(x.vec)]
      
      r <- runif(1, 0, 1)
      x <- x + (r < p) * 1.0 + (r > (1 - p)) * -1.0
      x.vec <- append(x.vec, x)
    }
    # print(x.vec)
    x.mat[, i] <- x.vec
  }
  # matplot(x.mat, type="l", lwd=1)
  
  results <- x.mat[n.flips,]
  n.breaks <- range(results)[2] - range(results)[1]

  hist(results, breaks=50)
  
  return(x.mat)
}

manyRandomWalks(1000, 2000, 0.5)


