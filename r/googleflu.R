library(mosaic)

# Read in the data
googleflu = read.csv("googleflu.csv", header=TRUE)

summary(googleflu)

# Remove the points where the outcome is missing
googleflu = na.omit(googleflu)


plot(cdcflu ~ week, data=googleflu)

# Build a predictive model
# Start with everything and work down!
# The dot stands for "everything not already named")
lmfull = lm(cdcflu ~ . - week, data=googleflu)

# Now backwards selection using AIC as an Occam's Razor
lmstep = step(lmfull, direction='both')
summary(lmstep)

# Compare the actual with the fitted values
plot(cdcflu ~ week, data=googleflu)
lines(fitted(lmstep) ~ week, data=googleflu)
lines(fitted(lmfull) ~ week, data=googleflu, col='red', lty='dotted')

# which variables were included?
lmstep$call

# Use cross validation to check generalization error versus a smaller model
Ntotal = nrow(googleflu)

# which points should be in my training set?
Ntrain = 150
trainset = sample(1:Ntotal, size=Ntrain)

# The remaining points should be in the test set
testset = setdiff(1:Ntotal, trainset)

# Fit the selected model to the training set
lmtrain.1 = lm(formula = cdcflu ~ how.long.does.flu.last + viral.pneumonia + 
    how.to.get.over.the.flu + signs.of.the.flu + flu.contagious + 
    can.dogs.get.the.flu + anas.barbariae.hepatis + cure.flu + 
    treat.flu + treatment.for.flu + remedies.for.flu + how.to.get.rid.of.the.flu + 
    symptoms.of.the.flu + cure.the.flu + low.body.temperature + 
    contagious.flu + painful.cough + flu.or.cold + thermoscan + 
    taking.temperature + fever.flu + oral.thermometer + treat.a.fever + 
    fever.reducers + reduce.fever + flu.remedies + symptoms.pneumonia + 
    what.is.influenza + incubation.period.for.the.flu + high.fever + 
    how.long.does.fever.last + chest.cough, data = googleflu[trainset,])
    
# Fit a different model to the training set
lmtrain.2 = lm(formula = cdcflu ~ how.long.does.flu.last + viral.pneumonia + 
    how.to.get.over.the.flu + signs.of.the.flu + flu.contagious + 
    what.is.influenza + incubation.period.for.the.flu + high.fever + 
    how.long.does.fever.last + chest.cough, data = googleflu[trainset,])

# Make predictions on the test/holdout set
testpred.1 = predict.lm(lmtrain.1, newdata=googleflu[testset,])
testpred.2 = predict.lm(lmtrain.2, googleflu[testset,])

# Check the scoreboard and see who won!
mean( (testpred.1 - googleflu$cdcflu[testset])^2 )
mean( (testpred.2 - googleflu$cdcflu[testset])^2 )

