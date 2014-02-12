library(mosaic)

# Fit a model that aggregates across leagues
plot(Log10Salary ~ BattingAverage, data=baseballsalary)
lm1 = lm(Log10Salary ~ BattingAverage, data=baseballsalary)
summary(lm1)
confint(lm1)

# Now look at the x-y relationship stratified by league
xyplot(Log10Salary ~ BattingAverage | Class, data=baseballsalary)

# Fit a model with dummy variables for the different leagues
lm2 = lm(Log10Salary ~ BattingAverage + Class, data=baseballsalary)
summary(lm2)

# What if you wanted the intercepts "straight up" rather than in baseline/offset?
# This is sometimes useful!
lm2b = lm(Log10Salary ~ BattingAverage + Class - 1, data=baseballsalary)
summary(lm2b)

# Look at one of the subsets with a dummy variable for Class
plot(Log10Salary ~ BattingAverage, data=subset(baseballsalary, Class=="AAA"))
abline(2.75937 +  1.03378, 5.69296)

# Fit a model with an interaction term between Class and Average
lm3 = lm(Log10Salary ~ BattingAverage + Class + Class:BattingAverage, data=baseballsalary)
summary(lm3)

# Look at the same subset with both a dummy variable and an interaction
plot(Log10Salary ~ BattingAverage, data=subset(baseballsalary, Class=="AAA"))
abline(2.8488 +  1.7936, 5.3985  -2.6468)

confint(lm3)

## Further optional plotting commands
plot(Log10Salary ~ BattingAverage, data=baseballsalary, las=1)
points(Log10Salary ~ BattingAverage, data=subset(baseballsalary, Class=='AA'), col='red', pch=19)
points(Log10Salary ~ BattingAverage, data=subset(baseballsalary, Class=='AAA'), col='blue', pch=19)
points(Log10Salary ~ BattingAverage, data=subset(baseballsalary, Class=='MLB'), col='black', pch=19)

lmAA = lm(Log10Salary ~ BattingAverage, data=subset(baseballsalary, Class=='AA'))
lmAAA = lm(Log10Salary ~ BattingAverage, data=subset(baseballsalary, Class=='AAA'))
lmMLB = lm(Log10Salary ~ BattingAverage, data=subset(baseballsalary, Class=='MLB'))
abline(lmAA, col='red')
abline(lmAAA, col='blue')
abline(lmMLB, col='black')
