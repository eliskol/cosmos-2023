# nextTerm <- function(x) r * (1 - x) * x

K <- 50

nextTerm <- function(x) 2 * x + (x ** 2) / K

identityLine <- function(x) x


r <- 3.25
startingTerm <- 0.2
iterations <- 100
xlims <- c(0, 1)
ylims <- c(0, 0.8)

plot(seq(xlims[1], xlims[2], by = 0.01), nextTerm(seq(xlims[1], xlims[2], by = 0.01)), xlim = xlims, ylim = ylims, type = "l", col = "red", xlab = "p_n", ylab = "p_(n+1)")
lines(seq(xlims[1], xlims[2], by = 0.01), identityLine(seq(xlims[1], xlims[2], by = 0.01)))

segments(startingTerm, 0, startingTerm, nextTerm(startingTerm), col = "green")

p_n <- startingTerm
p_n_plus1 <- nextTerm(p_n)

colfunc <- colorRampPalette(c("red", "orange"))
colors <- colfunc(2 * iterations)

for (i in 1:iterations) {
  segments(p_n, p_n_plus1, p_n_plus1, p_n_plus1, col = colors[(2 * i) - 1])
  p_n <- nextTerm(p_n)
  p_n_plus1 <- nextTerm(p_n)
  segments(p_n, p_n, p_n, p_n_plus1, col = colors[2 * i])
}