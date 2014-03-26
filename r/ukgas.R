ukgas = read.csv('ukgas.csv')

# Scrub missing values
ukgas = na.omit(ukgas)

# Gas consumption is growing over time
plot(ukgas$gas)

# Step 1: add a time index
N = nrow(ukgas)
ukgas$period = 1:N
head(ukgas)

lm1 = lm(gas ~ period, data=ukgas)
plot(resid(lm1))

# Residuals look more spread out over time: try a log transformation
lm2 = lm(log(gas) ~ period, data=ukgas)
plot(resid(lm2))

# Gas consumption is also growing versus the size of the population and economy
plot(gas~pop, data=ukgas)
plot(gas~log(pop), data=ukgas)
plot(gas~log(gdp), data=ukgas)

# We'll try adding these predictors to the model
lm3 = lm(log(gas) ~ period + log(pop) + log(gdp), data=ukgas)
summary(lm3)
plot(resid(lm3))

# We can safely drop the trend term
drop1(lm3)
lm4 = lm(log(gas) ~ log(pop) + log(gdp), data=ukgas)
summary(lm4)

# Big seasonal effects
boxplot(resid(lm4) ~ quarter, data=ukgas)

lm5 = lm(log(gas) ~ log(pop) + log(gdp) + factor(quarter), data=ukgas)
summary(lm5)

plot(resid(lm5))