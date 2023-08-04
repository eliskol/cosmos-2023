r <- 3.6
x <- 0.5

x_vec <- c()
next_term <- function(x) r * (1 - x) * x

num_terms <- 40000
 
for (i in 1 :num_terms) {
  x <- next_term(x)
  x_vec <- append(x_vec, x)
}

howManyLastTerms <- 63
truncatedX <- x_vec[(num_terms - howManyLastTerms): num_terms]

print(length((num_terms - howManyLastTerms):num_terms))
print(length(truncatedX))

plot(((num_terms - howManyLastTerms):num_terms), truncatedX, xlim=range(((num_terms - howManyLastTerms):num_terms)), ylim=range(truncatedX), col="red", type="p", pch="o")

for (i in 1:howManyLastTerms + 1) {
  lines(((num_terms - howManyLastTerms):num_terms), rep(truncatedX[i], howManyLastTerms + 1))
}



print("done")