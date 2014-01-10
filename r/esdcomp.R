library(mosaic)
library(faraway)

# Load data and look at the help file
data(esdcomp, package="faraway")
?esdcomp

# Exploratory analysis

# Complaints by visits
plot(complaints~visits, data=esdcomp)

# Complaints by residency status
stripchart(complaints~residency, data=esdcomp, method="jitter", vertical=TRUE)

# Complaints by sex
stripchart(complaints~gender, data=esdcomp, method="jitter", vertical=TRUE)

# Complaints by hours worked
plot(complaints~hours, data=esdcomp)


# Let's fit a model with all four of these factors

lm1 = lm(complaints ~ visits + residency + gender + hours, data=esdcomp)

# Extract the coefficients
# Spend some time thinking about their interpretation
coef(lm1)

# Estimate the sampling distributions of each coefficient via bootstrapping
# Will need the mosaic library for this command to work
myboot = do(1000)*lm(complaints ~ visits + residency + gender + hours, data=resample(esdcomp))
hist(myboot)

# Put the coefficients right next to the standard errors
coef(lm1)
sd(myboot)

# Now compute standard errors under the normal linear regression model
# Notice the standard errors are pretty similar to what you get under bootstrapping
summary(lm1)

# Finally, make predictions

# First import the "to be predicted" data set with specific values of the predictors

# Now use the predict.lm function
mypred = predict.lm(lm1, newdata=esdpredict, interval = "prediction", level=0.95)

# Put the predictions next to the design points to which they correspond
cbind(esdpredict, mypred)

# Notice the negative values!  A potential problem with the model here.
