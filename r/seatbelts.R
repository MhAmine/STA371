library(mosaic)

# seatbelts data
summary(seatbelts)

# Redefine the outcome as traffic deaths per 100 million miles
seatbelts$fatalities = 100*seatbelts$fatalities

# Fatalities differ state by state
boxplot(fatalities ~ state, data=seatbelts)

# Within each state, fatalities are related to year and miles
xyplot(fatalities ~ year | state, data=seatbelts)
xyplot(fatalities ~ miles | state, data=seatbelts)
xyplot(fatalities ~ seatbelt | state, data=seatbelts)

# Fatalities are lower in states with stricter speed limits and alcohol laws
boxplot(fatalities ~ speed65, data=seatbelts)
boxplot(fatalities ~ speed70, data=seatbelts)
boxplot(fatalities ~ drinkage, data=seatbelts)
boxplot(fatalities ~ alcohol, data=seatbelts)

# Fatalities are related to income
plot(fatalities ~ income, data=seatbelts)

# States with younger populations and stronger enforcement tend to have lower fatality rates
plot(fatalities ~ age, data=seatbelts)
boxplot(fatalities ~ enforce, data=seatbelts)

# OK then, let's start big!
lm1 = lm(fatalities ~ state + year + miles + seatbelt + speed65 + speed70 + drinkage + alcohol + income + age + enforce, data=seatbelts)

# shorthand: the . stands for "all variables not otherwise named"
lm2 = lm(fatalities ~ ., data=seatbelts)
summary(lm2)

# Some variables look like their effect is meager, once we adjust for other factors in the model
anova(lm2)

# We can probably drop enforcement.
# It has a small effect, and its effect is mediated by seatbelt usage!
lm3 = lm(fatalities ~ . - enforce, data=seatbelts)
summary(lm3)
anova(lm3)

# Neither drinkage nor alcohol look important, but they are highly collinear (=redundant information)
# Let's try dropping drinkage, since it is less precisely estimated.
lm4 = lm(fatalities ~ . - enforce - alcohol, data=seatbelts)
summary(lm4)
anova(lm4)

# drinkage still looks unimportant when adjusting for other factors, as does speed65
# Let's try dropping both
lm5 = lm(fatalities ~ . - enforce - alcohol - drinkage - speed65, data=seatbelts)
summary(lm5)
anova(lm5)

# R^2 barely budged, and now everything we have in looks important

# Next step: any important interactions? Remember the lattice plots
lm6 = lm(fatalities ~ state + year + miles + seatbelt + speed70 + income + age + state:seatbelt + state:miles, data=seatbelts)
summary(lm6)
anova(lm6)

# Model diagnostics
plot(lm6)
