library(mosaic)

# Read in the data, and tell R what column the row labels are in
# That way it doesn't think week is a categorical variable
googleflu = read.csv("googleflu.csv", header=TRUE)

summary(googleflu)

# Remove the points where the outcome is missing
googleflu = na.omit(googleflu)


plot(cdcflu ~ week, data=googleflu)

# Build a predictive model
# Start with everything and work down!
# The dot stands for "everything not already named")
lmfull = lm(cdcflu ~ . - week, data=googleflu)

# Now backwards selection using AIC as an Occam's Razor
lmstep = step(lmfull, direction='both')
summary(lmstep)

# Compare the actual with the fitted values
plot(cdcflu ~ week, data=googleflu)
lines(fitted(lmstep) ~ week, data=googleflu)
lines(fitted(lmfull) ~ week, data=googleflu, col='red', lty='dotted')
