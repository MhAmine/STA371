#### Galapagos Islands example

# Load data set
lm1 = lm(Species ~ Area, data = gala)
summary(lm1)

plot(Species ~ Area, data = gala)
# Ugly... needs a transformation

# log(Species) versus log(Area)
plot(log(Species) ~ log(Area), data = gala)
lm2 = lm(log(Species) ~ log(Area), data = gala)
abline(lm2)
summary(lm2)


# Now species versus elevation

lm3 = lm(log(Species) ~ log(Elevation), data = gala)
plot(log(Species) ~ log(Elevation), data = gala)
abline(lm3)


## Show that the predictors are collinear
plot(log(Area) ~ log(Elevation), data=gala, pch=19, bty='n')


## Two stage regressions

lm4a = lm(log(Species)~log(Area), data=gala)
summary(lm4a)

# Now regress the adjusted species count on log(Elevation)
lm4b = lm(resid(lm4a)~log(Elevation), data=gala)
summary(lm4b)

# What if we did this the other way around?
lm5a = lm(log(Species)~log(Elevation), data=gala)
summary(lm5a)

lm5b = lm(resid(lm5a)~log(Area), data=gala)
summary(lm5b)




# First construct an adjusted elevation measure
# "Take the area out of elevation"
lm6 = lm(log(Elevation)~log(Area), data=gala)
coef(lm6)
elevadj = resid(lm6)

# Now regress log species on adjusted elevation
# and look at the slope to estimate marginal effect, adjusting for area
lm7 = lm(log(Species)~elevadj, data=gala)
coef(lm7)

# Now in reverse... construct adjusted area
# "Take the elevation out of area"
lm8 = lm(log(Area)~log(Elevation), data=gala)
areaadj = resid(lm8)

# Now regress log species on adjusted area
# and look at the slope to estimate marginal effect, adjusting for elevation
lm9 = lm(log(Species)~areaadj, data=gala)
coef(lm9)

## Now in one stage, with multiple regression
## This fits a plane through the 3d point cloud!
lm10 = lm(log(Species)~log(Area) + log(Elevation), data=gala)
coef(lm10)



## Load the RGL library for 3d graphics
# This may not work on your computer without XQuartz installed
library(rgl)

## Now show the 3d plot
G3 = data.frame(LogElevation = log(gala$Elevation), LogArea = log(gala$Area), LogSpecies = log(gala$Species))
plot3d(G3, size=7)

### Now draw in the plane

lm.gala = lm(LogSpecies ~ LogElevation + LogArea, data=G3)
x1 = seq(min(G3$LogElevation), max(G3$LogElevation), length=20)
x2 = seq(min(G3$LogArea), max(G3$LogArea), length=20)
Xnew = expand.grid(x1,x2)
surface3d(x1,x2,predict.lm(lm.gala, data.frame(LogElevation = Xnew[,1], LogArea = Xnew[,2])), col="lightblue")

