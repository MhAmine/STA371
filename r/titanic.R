# Load in the relevant libraries
library(effects)
library(mosaic)

# Walk through the process of installing one of these

# Load in the data from the "effects" R package
data(TitanicSurvival, package="effects")

names(TitanicSurvival)

# To view the data in RStudio,
# double click on it in the Workspace pane,
# or just type the name of the data set.
TitanicSurvival

# Look at the first few lines
head(TitanicSurvival)

# Make tables that shows who survived, stratified by sex and cabin class
xtabs(~survived + sex, data=TitanicSurvival)
tally(~survived + sex, data=TitanicSurvival)
tally(~survived + passengerClass, data=TitanicSurvival)
tally(~survived + sex:passengerClass, data=TitanicSurvival)


# Define factors for age and age:sex interaction
AgeFactor = cut(TitanicSurvival$age, breaks=c(0,17,Inf))
AgeFactor = cut(TitanicSurvival$age, breaks=c(0,17,Inf), labels=c("Child", "Adult"))
AgeSexFactor = factor(AgeFactor:TitanicSurvival$sex)

# Make a table that shows who survived, stratified by age and sex
# Since some ages are missing (represented by NA's) in AgeFactor,
# this table won't classify everybody
tab1 = xtabs(~survived + AgeSexFactor, data=TitanicSurvival)
tab1
