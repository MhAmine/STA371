library(mosaic)

carauction = read.csv("carauction.csv", header=TRUE)
summary(carauction)

# Plot price cost versus age
plot(MMRAcquisitonRetailCleanPrice~VehicleAge, data=carauction, pch=19, col=rgb(20,20,20,10,maxColorVal=256))

# Start adding dummy variables corresponding to categorical predictions
lm1 = lm(MMRAcquisitonRetailCleanPrice ~ VehicleAge, data=carauction)
summary(lm1) 

# Dummy variables for the different makes
xtabs(~ Make, data=carauction)
lm2 = lm(MMRAcquisitonRetailCleanPrice ~ VehicleAge + Make, data=carauction)
summary(lm2) 

# Add variables for Color and Transmission
lm3 = lm(MMRAcquisitonRetailCleanPrice ~ VehicleAge + Make + Color + Transmission, data=carauction)
summary(lm3) 

# More dummies and a second numerical predictor: vehicle mileage
lm4 = lm(MMRAcquisitonRetailCleanPrice ~ VehicleAge + Make + Color + Transmission + IsOnlineSale + Auction + VNST + I(VehOdo/1000) , data=carauction)
summary(lm4) 

# Why not add more dummy variables for Model?
xtabs(~ Model, data=carauction)

# Histogram of the residuals
# Maybe non-Gaussian!
hist(resid(lm4), breaks=100, col='lightgrey')

# Residuals versus fitted values
plot(resid(lm4) ~ fitted(lm4), pch=19, col=rgb(20,20,20,20,maxColorVal=256))

# The conclusion: can't trust the standard errors or prediction intervals
# from the Gaussian regression model
