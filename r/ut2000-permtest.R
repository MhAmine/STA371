library(mosaic)
library(lattice)
library(ggplot2)

# Read in data
#ut2000 = read.csv('ut2000.csv', header=TRUE)

summary(ut2000)
# Rename the levels to avoid labels that YELL AT YOU
ut2000$School = factor(ut2000$School, labels=c("Architecture", "Business", "Communications", "Education", "Engineering", "Fine Arts", "Liberal Arts", "Natural Science", "Nursing", "Social Work"))

xyplot(GPA~SAT.C | School, data=ut2000)

# Let's compare models with and without dummies for college
lm1 = lm(GPA ~ SAT.C, data=ut2000)
lm2 = lm(GPA ~ SAT.C + School, data=ut2000)
coef(lm1)
coef(lm2)

# What statistic can we use to measure "discrepancy"?
permtest1 = do(1000)*lm(GPA ~ SAT.C + shuffle(School), data=ut2000)

hist(permtest1)
hist(permtest1$r.squared)


lm3 = lm(GPA ~ SAT.C + School + SAT.C:School, data=ut2000)
coef(lm3)

permtest2 = do(1000)*lm(GPA ~ SAT.C + School + SAT.C:shuffle(School), data=ut2000)
hist(permtest2$r.squared)
summary(lm3)