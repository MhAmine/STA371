library(mosaic)

## Bootstrapping

# Import data from marketmodel.csv
names(marketmodel)
lm1 = lm(WMT ~ SP500, data=marketmodel)
summary(lm1)

boot1 = do(1000)*lm(WMT ~ SP500, data=resample(marketmodel))

hist(boot1$SP500)
sd(boot1$SP500)

# Import ut2000.csv
mean(SAT.Q ~ School, data=ut2000)
lm(SAT.Q ~ School, data=ut2000)
boot2 = do(1000)*lm(SAT.Q ~ School, data=resample(ut2000))
confint(boot2)
hist(boot2[,7])

subArch = subset(ut2000, School=='ARCHITECTURE')
subLA = subset(ut2000, School=='LIBERAL ARTS')
mean(resample(subArch)$SAT.Q)
mean(resample(subLA)$SAT.Q)

boot3 = do(1000)*(mean(resample(subArch)$SAT.Q) - mean(resample(subLA)$SAT.Q))

head(boot3)
confint(boot3)




## Problem 2

# read in data on shocks
lm2 = lm(expensive ~ cheap, data=shocks)
plot(expensive ~ cheap, data=shocks)
# Passes the R^2 test
summary(lm2)

newdata = data.frame(cheap = c(510, 550, 590))


# Too wide at the high end
predict(lm2, newdata, interval='prediction', level = 0.95)
615.3066 - 581.5288

# need Gaussianity assumption
hist(resid(lm2))

# Just a spot-check comparison with 35 random normal draws
hist(rnorm(35, 0, 7.692686))

# A useful diagnostic is a QQ plot
plot(lm2)


# Utilities data
utilities = read.csv('http://jgscott.github.com/STA371/data/utilities.csv')

N = nrow(utilities)
Ntrain = 59  ## About 50% of the data used to train, 50% to test
Ntest = N - Ntrain

# Sample the overall data set to create a training set
trainset = sample(1:N, Ntrain)

# Pick out the appropriate rows of the data frame for the training set
util.train = utilities[trainset,]

# And the rest for the test set
util.test = utilities[-trainset,]

# Fit models using only the training data
lm1 = lm(gasbill~temp,data=util.train)
lm2 = lm(gasbill~temp+I(temp^2),data=util.train)

# Predict on the testing data
pred1 = predict(lm1,newdata=util.test)
pred2 = predict(lm2,newdata=util.test)

# Compute each model's mean-squared error
mse1 = mean((pred1 - util.test$gasbill)^2)
mse2 = mean((pred2 - util.test$gasbill)^2)

# Create a placeholder matrix
nSims = 100
mysim = matrix(0, nrow=nSims, ncol=4)
for(i in 1:nSims)
{
  # Sample a new split of the data set
  trainset = sample(1:N, Ntrain)
  
  # Pick out the appropriate rows of the data frame for the train/test sets
  util.train = utilities[trainset,]
  util.test = utilities[-trainset,]
  
  # Fit models using only the training data
  lm1 = lm(gasbill~temp,data=util.train)
  lm2 = lm(gasbill~temp+I(temp^2),data=util.train)
  lm3 = lm(gasbill~temp + I(temp^2) + I(temp^3), data=util.train)
  lm4 = lm(gasbill~temp + I(temp^2) + I(temp^3) + I(temp^4),data=util.train)
  
  # Predict on the testing data
  pred1 = predict(lm1,newdata=util.test)
  pred2 = predict(lm2,newdata=util.test)
  pred3 = predict(lm3,newdata=util.test)
  pred4 = predict(lm4,newdata=util.test)
  
  # Compute each model's mean-squared error
  mse1 = mean((pred1 - util.test$gasbill)^2)
  mse2 = mean((pred2 - util.test$gasbill)^2)
  mse3 = mean((pred3 - util.test$gasbill)^2)
  mse4 = mean((pred4 - util.test$gasbill)^2)
  
  # Store the result for this split
  mysim[i,] = c(mse1,mse2,mse3,mse4)
}




do(100)*{
  # Sample a new split of the data set
  trainset = sample(1:N, Ntrain)
  
  # Pick out the appropriate rows of the data frame for the train/test sets
  util.train = utilities[trainset,]
  util.test = utilities[-trainset,]
  
  # Fit models using only the training data
  lm1 = lm(gasbill~temp,data=util.train)
  lm2 = lm(gasbill~temp+I(temp^2),data=util.train)
  lm3 = lm(gasbill~temp + I(temp^2) + I(temp^3), data=util.train)
  lm4 = lm(gasbill~temp + I(temp^2) + I(temp^3) + I(temp^4),data=util.train)
  
  # Predict on the testing data
  pred1 = predict(lm1,newdata=util.test)
  pred2 = predict(lm2,newdata=util.test)
  pred3 = predict(lm3,newdata=util.test)
  pred4 = predict(lm4,newdata=util.test)
  
  # Compute each model's mean-squared error
  mse1 = mean((pred1 - util.test$gasbill)^2)
  mse2 = mean((pred2 - util.test$gasbill)^2)
  mse3 = mean((pred3 - util.test$gasbill)^2)
  mse4 = mean((pred4 - util.test$gasbill)^2)
  
  # Store the result for this split
  c(mse1,mse2,mse3,mse4)
}


head(mysim)
colMeans(mysim)

