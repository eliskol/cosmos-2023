a <- 3 / 4
b <- 300 / 4
x <- 500

x_vec <- c()
next_term <- function(x) a * x + b

num_terms <- 50

for (i in 1 :num_terms) {
  x <- next_term(x)
  x_vec <- append(x_vec, x)
}

print(x_vec)
plot(1:num_terms, x_vec, xlim=range(1:num_terms), ylim=range(x_vec), col="red", type="p")

# x <- 10
# a <- 0.5

# next_term <- function(x) a * x

# for (i in 1:10) {
#   x <- next_term(x)
#   print(x)
# }
