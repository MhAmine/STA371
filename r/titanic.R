# Load in the relevant libraries
library(effects)
library(graphics)
library(mosaic)

# Walk through the process of installing one of these

# Load in the data from the "effects" R package
data(Titanic, package="effects")

names(Titanic)

# To view the data in RStudio,
# double click on it in the Workspace pane,
# or just type the name of the data set.
Titanic

# Rename the factor
SurvivalStatus = factor(Titanic$survived)


# The labels could be more informative
SurvivalStatus = factor(Titanic$survived, levels=c("yes", "no"), labels=c("Survived", "Died"))

# Define factors for age and age:sex interaction
AgeFactor = cut(Titanic$age, breaks=c(0,17,Inf))
AgeSexFactor = factor(AgeFactor:Titanic$sex)

# The labels aren't pretty! Let's rename them.
AgeFactor = cut(Titanic$age, breaks=c(0,17,Inf), labels=c("Child", "Adult"))
AgeSexFactor = factor(AgeFactor:Titanic$sex, labels=c("Girl", "Boy", "Adult female", "Adult male"))

# Make a table that shows who survived, stratified by age and sex
# Since some ages are missing (represented by NA's) in AgeFactor,
# this table won't classify everybody
table(SurvivalStatus,AgeSexFactor)


# Make a table that shows who survived, stratified by sex and cabin class
table(SurvivalStatus,Titanic$sex:Titanic$passengerClass)


# Make a mosaic plot that shows who survived, stratified by age and sex
mosaicplot(AgeSexFactor~SurvivalStatus, col=c("lightgrey", "darkred"), main="Survival of Titanic passengers by age and sex", xlab="", cex.axis=1.5)

