bballbets = read.csv("bballbets.csv", header=TRUE)

plot(jitter(homewin) ~ spread, data=bballbets)

# Look at the empirical win frequency within "buckets"
spread.discrete = cut(bballbets$spread, breaks=seq(-35,45,by=10))
lm0 = lm(homewin~spread.discrete, data=bballbets)
plot(jitter(homewin) ~ spread, data=bballbets)
points(fitted(lm0) ~ spread, data=bballbets, col='blue', pch=19)


#### Fit a model to win versus spread
lm1 = lm(homewin~spread, data=bballbets)
summary(lm1)
plot(jitter(homewin) ~ spread, data=bballbets)
abline(lm1)

# Uh oh! probabilities outside [0,1] are silly...
hist(fitted(lm1))

# Try plotting versus the original x variable
plot(fitted(lm1)~spread,data=bballbets)
ablin

# Now fit a logistic regression model
glm1 = glm(homewin~spread, data=bballbets, family=binomial)

# Plot these probabilities versus the original x variable
# See the sigmoidal curve.
plot(fitted(glm1)~spread,data=bballbets)

# Could also superimpose the original linear fit on top
plot(fitted(lm1)~spread, data=bballbets, col='red', pch=19)
points(fitted(glm1)~spread,data=bballbets, col='blue', pch=19)

legend("topleft", legend=c("Linear", "Logistic"), col=c("red", "blue"), pch=19)
