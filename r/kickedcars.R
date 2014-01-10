library(mosaic)

cars = read.csv("kickedcars.csv", header=TRUE)


dim(cars)
summary(cars)

table(cars$IsBadBuy,cars$Make)

lm1 = lm(WarrantyCost~I(VehOdo/1000), data=cars)

plot(WarrantyCost~I(VehOdo/1000), data=cars, pch=19, col=rgb(20,20,20,10,maxColorVal=256))
abline(lm1, col='red')

summary(lm1)

# Notice the fan
plot(resid(lm1)~I(VehOdo/1000), data=cars, pch=19, col=rgb(20,20,20,10,maxColorVal=256))

# Histogram of the residuals
# Highly non-Gaussian!
hist(resid(lm1), breaks=100, col='lightgrey')

# A lot of these diagnostic plots are invoked by plotting the linear model object
plot(lm1)

# Regression which is robust to outliers
# Technically Huber's M estimator
# There are many, many flavors of this!
library(MASS)
rlm1 = rlm(WarrantyCost~I(VehOdo/1000), data=cars, method='M')
summary(rlm1)

# Plot the residuals versus the weights used in weighted least squares
plot(resid(rlm1), rlm1$w)
