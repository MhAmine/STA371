## Method 1: plot realized profit vs price and fit a quadratic

profit = milk$sales * (milk$price - 1.50)
plot(profit ~ price, data=milk)

lm2 = lm(profit ~ price + I(price^2), data=milk)
points(fitted(lm2) ~ price, data=milk, col='blue')





## Method 2: fit a demand curve and use this in the equation for profit
plot(log(sales) ~ log(price), data=milk)
lm1 = lm(log(sales) ~ log(price), data=milk)
abline(lm1)

coef(lm1)
beta = coef(lm1)

plot(sales~price, data=milk)
curve(exp(beta[1])*x^beta[2], add=TRUE, col='blue')

# Now use R just like a graphing calculator
unitcost = 1.50
curve( (x-unitcost)*exp(beta[1])*x^beta[2], from=1, to=10)
curve( (x-unitcost)*exp(beta[1])*x^beta[2], from=3, to=6)


xgrid = seq(1,10,length=1000)
profitgrid = (xgrid-unitcost)*exp(beta[1])*xgrid^beta[2]
which.max(profitgrid)
xgrid[326]
