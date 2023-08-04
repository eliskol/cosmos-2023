n.boxes <- 10
p <- 0.5
n.trials <- 10
x.start <- 0



colfunc <- colorRampPalette(c("red", "purple"))
colors <- topo.colors(n.trials)

for (i in 1 : n.trials) {
  x.vec <- c(x.start)
  while (x.vec[length(x.vec)] < n.boxes) {
    r <- runif(1, 0, 1)
    x <- x.vec[length(x.vec)] + (r < p) * 1.0 + (x.vec[length(x.vec)] > x.start) * (r > (1 - p)) * -1.0
    x.vec <- append(x.vec, x)
  }
  
  if (i == 1) {
    plot(x.vec, type="l", col=colors[1])
  }
  lines(x.vec, col=colors[i], xlim=c(1, length(x.vec)))
  print(length(x.vec))
}