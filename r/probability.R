library(mosaic)

# Probability mass function of a binomial
p_noshow = 0.05
passengers = 195

# How likely is it that 5 passengers won't show up?
dbinom(5, passengers, p_noshow)

# What about every possible value between 0 and the number of passengers?
x = 0:passengers  # Create a grid
pmf1 = dbinom(x, passengers, p_noshow)
plot(x, pmf1)

# Notice that pmf1 must sum to 1
sum(pmf1)


# Probability mass function of a Poisson
callrate = 10

# How likely is it that 17 people will call in the next minute?
dpois(10, callrate)

# What about every possible value between 0 and 30?
x = 0:30  # Create a grid
pmf2 = dpois(x, callrate)
plot(x, pmf2)

# Notice that pmf2 doesn't quite sum to 1...
sum(pmf2)

