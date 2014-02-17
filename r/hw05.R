library(mosaic)


# Problem 2

# Read in data
consumerexp = read.csv('consumerexp.csv')
consumerexp

# Plot consumer expenditures over time
plot(consumerexp$Expenditure)

# Could also add an explicit time index
consumerexp$Period = 1:20
plot(Expenditure ~ Period, data=consumerexp)


# Plot consumer expenditures versus stock of money
plot(Expenditure ~ Stock, data=consumerexp)

# Fit a line
lm1 = lm(Expenditure ~ Stock, data=consumerexp)
abline(lm1)

# Look at the residuals over time
plot(resid(lm1) ~ Period, data=consumerexp)



# Problem 3

cheese = read.csv('cheese.csv')
head(cheese)
summary(cheese)

xtabs(~store, data=cheese)

boxplot(vol~disp, data=cheese)
lm1 = lm(vol~disp, data=cheese)
summary(lm1)

# Look store by store
boxplot(vol~disp, data=subset(cheese, store=='BALTI/WASH - SAFEWAY'))
boxplot(vol~disp, data=subset(cheese, store=='SYRACUSE - PRICE CHOPPER'))

# Put in dummy variables store by store
lm2 = lm(vol~disp + store, data=cheese)
summary(lm2)

# Now look at price
plot(vol~price, data=cheese)
points(vol~price, data=subset(cheese, disp==1), col='blue', pch=19)
points(vol~price, data=subset(cheese, disp==0), col='red', pch=19)
legend("topright", legend=c("Display", "No display"), pch=19, col=c('blue', 'red'))

# Store by store
plot(vol~price, data=subset(cheese, store=='BALTI/WASH - SAFEWAY'))
plot(vol~price, data=subset(cheese, store=='SYRACUSE - PRICE CHOPPER'))

# On a log scale
plot(log(vol)~log(price), data=cheese)

# Store by store?
plot(log(vol)~log(price), data=subset(cheese, store=='BALTI/WASH - SAFEWAY'))
plot(log(vol)~log(price), data=subset(cheese, store=='SYRACUSE - PRICE CHOPPER'))

lm3 = lm(log(vol)~log(price) + store, data=cheese)
summary(lm3)

# Spot check
plot(log(vol)~log(price), data=subset(cheese, store=='DALLAS/FT. WORTH - KROGER CO'))
abline(9.56181 + 1.48359, -2.64380)

plot(vol~price, data=subset(cheese, store=='DALLAS/FT. WORTH - KROGER CO'))
curve(exp(9.56181 + 1.48359)*x^(-2.64380), add=TRUE)


# What about the effect of displays?
lm4 = lm(log(vol)~log(price) + store + disp, data=cheese)
summary(lm4)

dfwkroger = subset(cheese, store=='DALLAS/FT. WORTH - KROGER CO')
sub1 = subset(dfwkroger, disp==1)
sub0 = subset(dfwkroger, disp==0)
plot(vol~price, data=dfwkroger)
points(vol~price, data=sub1, col='blue', pch=19)
points(vol~price, data=sub0, col='red', pch=19)
curve(exp(9.37579 + 1.43461)*x^(-2.53159), add=TRUE, col='red')
curve(exp(9.37579 + 1.43461 + 0.18540)*x^(-2.53159), add=TRUE, col='blue')



