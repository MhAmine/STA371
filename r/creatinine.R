library(mosaic)
#creatinine = read.csv('creatinine.csv')

N = nrow(creatinine)

plot(creatclear~age, data=creatinine)
lm1 = lm(creatclear~age, data=creatinine)
summary(lm1)

boot1 = do(1000)*lm(creatclear~age, data=resample(creatinine))

hist(boot1$age)
sd(boot1$age)


# Create two subsets: one for learning and one for prediction
trainset = sample(1:N, 80, replace=FALSE)

creatinine.train = creatinine[trainset,]
creatinine.test = creatinine[-trainset,]

# The training set looks like this:
plot(creatclear ~ age, data=creatinine.train)

# Fit a model on the training set
lm1 = lm(creatclear ~ age, data=creatinine.train)
summary(lm1)


# Make predictions on the test set
pred.test = predict(lm1, newdata=creatinine.test)

# Plot the predictions
plot(pred.test ~ age, data=creatinine.test)

# Make predictions on the test set with error bars
pred2.test = predict(lm1, newdata=creatinine.test, interval='prediction', level=0.95)

# Inspect the result
head(pred2.test)
pred2.test[,1]

# X marks the prediction intervals
plot(pred2.test[,1] ~ age, data=creatinine.test, pch=19)
points(pred2.test[,2] ~ age, data=creatinine.test, pch=4)
points(pred2.test[,3] ~ age, data=creatinine.test, pch=4)

# Zooming out a bit
plot(pred2.test[,1] ~ age, data=creatinine.test, pch=19, ylim=range(pred2.test))
points(pred2.test[,2] ~ age, data=creatinine.test, pch=4)
points(pred2.test[,3] ~ age, data=creatinine.test, pch=4)

# Add the actual points
points(creatclear ~ age, data=creatinine.test, pch=19, col='red')


