library(mosaic)


# Problem 1
# read in house data
names(house)
plot(price ~ sqft, data=house)

# We will clearly need dummy variables here
xyplot(price ~ sqft | nbhd, data=house)

# A clear aggregation paradox if we don't!
lm1 = lm(price ~ sqft, data=house)
summary(lm1)

lm2 = lm(price ~ sqft + nbhd, data=house)
summary(lm2)

# An interaction?
lm3 = lm(price ~ sqft + nbhd + nbhd:sqft, data=house)
summary(lm3)

# Notice the wide confidence intervals and very modest boost in R^2

# What if we add a dummy for brick?
# It looks like the interaction terms get much smaller
lm4 = lm(price ~ sqft + nbhd + nbhd:sqft + brick, data=house)
summary(lm4)
coef(lm4)
round(coef(lm4), 1)

# Add bedrooms and bathrooms?
lm5 = lm(price ~ sqft + nbhd + nbhd:sqft + brick + bedrooms + bathrooms, data=house)
summary(lm5)

# Drop the interaction?
lm6 = lm(price ~ sqft + nbhd + brick + bedrooms + bathrooms, data=house)
summary(lm6)


# Problem 2

#cheese = read.csv('cheese.csv')
#head(cheese)
#summary(cheese)


# What about the effect of displays? our model from last week
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


lm5 = lm(log(vol)~log(price) + store + disp + disp:log(price), data=cheese)
summary(lm5)
# Visualize the model again
mysub = subset(cheese, store=='ATLANTA - KROGER CO')
sub1 = subset(mysub, disp==1)
sub0 = subset(mysub, disp==0)
plot(vol~price, data=mysub)
points(vol~price, data=sub1, col='blue', pch=19)
points(vol~price, data=sub0, col='red', pch=19)
curve(exp(9.26095 + 1.62211)*x^(-2.42583), add=TRUE, col='red')
curve(exp(9.26095 + 1.62211 + 0.34153)*x^(-2.53159 - 0.14761), add=TRUE, col='blue')

plot(log(vol) ~ fitted(lm5), data=cheese)
abline(0,1)

plot(vol ~ exp(fitted(lm5)), data=cheese)




## Problem 3

names(georgia2000)

# Create the outcome variable
georgia2000$ucount = georgia2000$ballots - georgia2000$votes
boxplot(ucount ~ equip, data=georgia2000)

# Undercount rate
georgia2000$ucr = georgia2000$ucount/georgia2000$ballots

boxplot(ucr ~ equip, data=georgia2000)
boxplot(ucr ~ urban, data=georgia2000)
boxplot(ucr ~ poor, data=georgia2000)

lm1 = lm(ucr ~ urban + poor, data=georgia2000)
summary(lm1)

boxplot(resid(lm1) ~ equip, data=georgia2000)

lm2 = lm(ucr ~ urban + poor + equip, data=georgia2000)
summary(lm2)

lm3 = lm(ucr ~ urban + poor + equip + bush, data=georgia2000)
summary(lm3)



# Shuffling equipment: a permutation test
lm(ucr ~ urban + poor + shuffle(equip), data=georgia2000)

do(10)*lm(ucr ~ urban + poor + shuffle(equip), data=georgia2000)

perm1 = do(1000)*lm(ucr ~ urban + poor + shuffle(equip), data=georgia2000)
hist(perm1$r.squared)


