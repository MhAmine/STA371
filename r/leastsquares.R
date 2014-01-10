library(mosaic)

# Read in Edwin Hubble's original data set in hubble.txt
#hubble = read.table('hubble.txt', header=TRUE)

names(hubble)

# Plot the data
plot(recvelocity~distance, data=hubble)

# Fit a straight line by least squares
lm1 = lm(recvelocity~distance,data=hubble)

# Add the least-squares line to the current plot
abline(lm1)

# Extract the model coefficients
coef(lm1)


# Load the data on bacterial count after irradiation
#bacteria = read.table('bacteria.txt', header=TRUE)
names(bacteria)
plot(count~generation, data=bacteria)

# Clearly not a linear fit.  What to do?
plot(log(count)~generation, data=bacteria)

# Try a transformation
# The model is linear, just not in the original variables
lm2 = lm(log(count)~generation, data=bacteria)
abline(lm2)

# Extract the coefficients
coef(lm2)

# So the estimated model is y = exp(0.5 + 0.4*generation)

# what if we wanted to plot things on the original scale?
plot(count~generation, data=bacteria)
abline(lm2)




# That doesn't look right.
# We don't have a line back on the original scale
# Solution: undo the transformation to the fitted values
predictedlogcount = fitted(lm2)
plot(count~generation, data=bacteria)
lines(bacteria$generation, exp(predictedlogcount))



# Load the data set on body vs brain weights
library(faraway)
data(mammalsleep)

plot(brain~body, data=mammalsleep)
plot(log(brain)~body, data=mammalsleep)

# What else should we try?


# GDP growth data
gdpgrowth = read.csv("gdpgrowth.csv", header=TRUE)

# Try a linear model for GDP growth vs life expectancy
plot(GR6096~LIFE60, data=gdpgrowth)
lm.life60 = lm(GR6096~LIFE60, data=gdpgrowth)
abline(lm.life60)

# Really the data look like a curve
# Try adding a quadratic term
plot(GR6096~LIFE60, data=gdpgrowth)
lm.life60.quad = lm(GR6096~LIFE60 + I(LIFE60^2), data=gdpgrowth)

# Add the fitted values using points()
points(gdpgrowth$LIFE60, fitted(lm.life60.quad), pch=19, col='blue')

# This captures the ``leveling off" effect,
# but the downwards turn at the right is goofy

