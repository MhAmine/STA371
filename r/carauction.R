library(mosaic)

cars = read.csv("carauction.csv", header=TRUE)
summary(cars)

# Plot warranty cost versus mileage
plot(WarrantyCost~I(VehOdo/1000), data=cars, pch=19, col=rgb(20,20,20,10,maxColorVal=256))

# Start adding dummy variables corresponding to categorical predictions
lm1 = lm(WarrantyCost~I(VehOdo/1000), data=cars)
summary(lm1)

lm2 = lm(WarrantyCost~I(VehOdo/1000) + Make, data=cars)
summary(lm2)

lm3 = lm(WarrantyCost~I(VehOdo/1000) + Make + Transmission, data=cars)
summary(lm3)

# and on and on until we construct a good predictive model
mycar = data.frame(VehOdo = 50000, Make = "TOYOTA", Transmission = "AUTO")
predict(lm3, mycar)

# Careful with the prediction interval though: notice the fan!
plot(resid(lm3)~I(VehOdo/1000), data=cars, pch=19, col=rgb(20,20,20,10,maxColorVal=256))

# Histogram of the residuals
# Highly non-Gaussian!
hist(resid(lm3), breaks=100, col='lightgrey')

# The conclusion: can't trust the standard errors or prediction intervals
# from the Gaussian regression model
