# Markov chain simulator

# inputs

P <- rbind(c(7/8, 1/8),
           c(1/4, 3/4))

#params

X1 <- 1
Tf <- 200
reps <- 7

#output
X.mat <- matrix(X1, nrow=Tf, ncol=reps)

#running the sim
for (i in 1:reps) {
  for (n in 2: Tf) {
   X.mat[n, i] <- sample.int(n=dim(P)[1], size=1, prob=P[X.mat[n-1,i],])
  }
}

plot(X.mat, type = "l")