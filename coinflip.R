coinflip <- function() {
  x1 <- runif(1, 0, 1)
  if (x1 > 0.5) {
    yt <- 1
  } else {
    yt <- 0
  }
  return(yt)
}

bruh <- c()
for (i in 1:100) {
  bruh <- append(bruh, coinflip())
}
print(bruh)
colors <- rep("red", length(bruh))
colors[bruh > 0.5] <- "green"
colors[bruh < 0.5] <- "red"
hist(bruh, breaks=2, col=c("red", "green"))