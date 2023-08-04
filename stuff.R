a_n <- function(n) (1 / n) * sin(n)

sigma <- function(n) {
    if (n == 1) {
        output <- 1
        while(a_n(output) <= 0) {
            output <- output + 1
        }
        return(output)
    }
    prev <- sigma(n - 1)
    output <- prev + 1
    while (a_n(prev) <= a_n(output) <= 0) {
        output <- output + 1
    }
    return output
}

calculate_n_sigma_values <- function(n) {
    if (n == 1) {
        value = 1
        while (a_n(value) <= 0) {
            value <- value + 1
        }
        return value
    }
    arr <- calculate_n_sigma_values(n - 1)
    prev <- arr[length(arr) - 1]
    value <- prev + 1

    while (a_n(prev) >= a_n(value) >= 0) {
        if (n > 17) {
            print(value)
        }
        value <- value + 1
    }
    append(arr, value)
    return(arr)
}

for (i in 1:10) {
    print(a_n(i))
}