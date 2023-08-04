covidData <- read.csv(file="us-states.csv")
CA <- which(covidData$state == "California")
CA.data <- covidData[CA, ]
plot(CA.data$cases, type="l")
n <- length(CA)
new.cases <- CA.data$cases[-1] - CA.data$cases[-n]
plot(new.cases, type="l", log="y") # log=y makes it logarithmic plot

# find best fitting line for days 35-65

days <- 281:321
y <- log(new.cases[days])

coefficients <- lm(y~days)$coefficients

print(log(2) / coefficients[2])

# x_n = x_0 e ** (0.2 * n)