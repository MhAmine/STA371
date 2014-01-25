library(mosaic)

# Read in two data sources: traffic deaths and state names
# mrall = traffic deaths per 10000 residents
# vmiles = average miles per driver
# perinc = per capita personal income
# beertax = tax on a case of beer

# Merge the two data sets
traffic2 = merge(trafficdeaths, fips, by.x = "state", by.y="fipsnum")

# Define new variables that aggregate a state's statistic across years
frmean = mean(mrall~fipsalpha, data=traffic2)
milesmean = mean(vmiles~fipsalpha, data=traffic2)
incmean = mean(perinc~fipsalpha, data=traffic2)
taxmean = mean(beertax~fipsalpha, data=traffic2)

# Plot the combined variables
plot(frmean~milesmean)
lm1 = lm(frmean~milesmean)
abline(lm1)

plot(frmean~incmean)
lm2 = lm(frmean~incmean)
abline(lm2)

# Suppress the points and add text labels instead
plot(frmean~milesmean, data=traffic2, type='n',
     xlab='Average miles per year (10000)',
     ylab='Fatality rate per 10000 people')
text(frmean~milesmean, labels=names(frmean), cex=0.6, col='darkgreen')

# Stratify by whether the state has mandatory jail for drunk driving
xyplot(frmean~milesmean | jaild, data=traffic2)
